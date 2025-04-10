from __future__ import annotations

import json
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Dict, List, Tuple, Union


class SExprError(Exception):
    pass


class SExprParseError(SExprError):
    def __init__(self, message: str, token: Token | None = None) -> None:
        super().__init__(message)
        self.token = token


class ConfigValidationError(SExprError):
    pass


@dataclass(frozen=True)
class Config:
    indent_width: int = 2
    newline_after_open_paren: bool = True
    block_forms: Dict[str, int] = field(
        default_factory=lambda: {"define": 2, "let": 1, "lambda": 1}
    )
    newline_between_list_elements: bool = True
    space_before_comment: int = 2
    max_line_length: int = 80
    inline_threshold: int = 60
    preserve_comments: bool = True

    @classmethod
    def load(cls, filepath: str = ".scmfmt.json") -> Config:
        if os.path.exists(filepath):
            try:
                with open(filepath, encoding="utf-8") as f:
                    data = json.load(f)
            except Exception as e:
                raise ConfigValidationError(f"Error loading config: {e}") from e
            try:
                return cls(
                    indent_width=int(data.get("indent_width", 2)),
                    newline_after_open_paren=bool(
                        data.get("newline_after_open_paren", True)
                    ),
                    block_forms=dict(
                        data.get("block_forms", {"define": 2, "let": 1, "lambda": 1})
                    ),
                    newline_between_list_elements=bool(
                        data.get("newline_between_list_elements", True)
                    ),
                    space_before_comment=int(data.get("space_before_comment", 2)),
                    max_line_length=int(data.get("max_line_length", 80)),
                    inline_threshold=int(data.get("inline_threshold", 60)),
                    preserve_comments=bool(data.get("preserve_comments", True)),
                )
            except Exception as e:
                raise ConfigValidationError(f"Invalid configuration values: {e}") from e
        else:
            return cls()


@dataclass(frozen=True)
class Token:
    typ: str
    value: str
    line: int
    col: int


class Tokenizer:
    def __init__(self, source: str) -> None:
        self.source = source

    def tokenize(self) -> List[Token]:
        tokens: List[Token] = []
        lines = self.source.splitlines()
        for line_num, line in enumerate(lines, start=1):
            if line.strip() == "":
                tokens.append(Token("EMPTYLINE", "", line_num, 1))
            elif line.strip().startswith("#lang"):
                tokens.append(Token("LANG_DIRECTIVE", line.strip(), line_num, 1))
            else:
                tokens.extend(self._tokenize_line(line, line_num))
        return tokens

    def _tokenize_line(self, line: str, line_num: int) -> List[Token]:
        tokens: List[Token] = []
        i = 0
        while i < len(line):
            c = line[i]
            col = i + 1
            if c in " \t\r":
                j = i
                while j < len(line) and line[j] in " \t\r":
                    j += 1
                tokens.append(Token("WHITESPACE", line[i:j], line_num, col))
                i = j
            elif c == ";":
                tokens.append(Token("COMMENT", line[i:], line_num, col))
                break
            elif c == "(":
                tokens.append(Token("LPAREN", "(", line_num, col))
                i += 1
            elif c == ")":
                tokens.append(Token("RPAREN", ")", line_num, col))
                i += 1
            elif c == "[":
                tokens.append(Token("LPAREN", "[", line_num, col))
                i += 1
            elif c == "]":
                tokens.append(Token("RPAREN", "]", line_num, col))
                i += 1
            elif c == '"':
                j = i + 1
                escaped = False
                str_val = '"'
                while j < len(line):
                    ch = line[j]
                    str_val += ch
                    if escaped:
                        escaped = False
                    else:
                        if ch == "\\":
                            escaped = True
                        elif ch == '"':
                            break
                    j += 1
                else:
                    raise SExprParseError(
                        "Unterminated string literal",
                        Token("STRING", line[i:], line_num, col),
                    )
                j += 1
                tokens.append(Token("STRING", str_val, line_num, col))
                i = j
            elif c == "'":
                tokens.append(Token("QUOTE", "'", line_num, col))
                i += 1
            else:
                j = i
                while j < len(line) and line[j] not in ' \t\r();"':
                    j += 1
                tokens.append(Token("ATOM", line[i:j], line_num, col))
                i = j
        return tokens


def filter_tokens(tokens: List[Token]) -> List[Token]:
    return [tok for tok in tokens if tok.typ not in ("WHITESPACE",)]


@dataclass
class Node:
    start_line: int
    start_col: int
    end_line: int
    end_col: int


@dataclass
class Atom(Node):
    value: str

    def __str__(self) -> str:
        return self.value


@dataclass
class String(Node):
    value: str

    def __str__(self) -> str:
        content = self.value[1:-1]
        return f'"{content}"'


@dataclass
class Comment(Node):
    value: str

    def __str__(self) -> str:
        return self.value


@dataclass
class EmptyLine(Node):
    def __str__(self) -> str:
        return ""


@dataclass
class QuoteNode(Node):
    expr: Node

    def __str__(self) -> str:
        return "'" + str(self.expr)


@dataclass
class ListNode(Node):
    elements: List[Node]

    def __str__(self) -> str:
        return "(" + " ".join(str(e) for e in self.elements) + ")"


@dataclass
class LangDirective(Node):
    value: str

    def __str__(self) -> str:
        return self.value


class Parser:
    def __init__(self, tokens: List[Token]) -> None:
        self.tokens = tokens
        self.pos = 0

    def parse(self) -> List[Node]:
        ast: List[Node] = []
        while self.pos < len(self.tokens):
            ast.append(self._parse_expr())
        return ast

    def _parse_expr(self) -> Node:
        if self.pos >= len(self.tokens):
            raise SExprParseError("Unexpected end of tokens")
        token = self.tokens[self.pos]
        typ = token.typ
        val = token.value
        if typ == "LANG_DIRECTIVE":
            self.pos += 1
            return LangDirective(
                start_line=token.line,
                start_col=token.col,
                end_line=token.line,
                end_col=token.col + len(val),
                value=val,
            )
        elif typ == "QUOTE":
            self.pos += 1
            quoted = self._parse_expr()
            return QuoteNode(
                start_line=token.line,
                end_line=quoted.end_line,
                start_col=token.col,
                end_col=quoted.end_col,
                expr=quoted,
            )
        elif typ == "LPAREN":
            start_token = token
            self.pos += 1
            elements: List[Node] = []
            while self.pos < len(self.tokens) and self.tokens[self.pos].typ != "RPAREN":
                elements.append(self._parse_expr())
            if self.pos >= len(self.tokens) or self.tokens[self.pos].typ != "RPAREN":
                raise SExprParseError("Missing closing parenthesis", start_token)
            self.pos += 1  # Consume RPAREN.
            return ListNode(
                start_line=start_token.line,
                start_col=start_token.col,
                end_line=self.tokens[self.pos - 1].line,
                end_col=self.tokens[self.pos - 1].col,
                elements=elements,
            )
        elif typ == "RPAREN":
            raise SExprParseError("Unexpected RPAREN", token)
        elif typ == "ATOM":
            self.pos += 1
            return Atom(
                start_line=token.line,
                start_col=token.col,
                end_line=token.line,
                end_col=token.col + len(val),
                value=val,
            )
        elif typ == "STRING":
            self.pos += 1
            return String(
                start_line=token.line,
                start_col=token.col,
                end_line=token.line,
                end_col=token.col + len(val),
                value=val,
            )
        elif typ == "COMMENT":
            self.pos += 1
            return Comment(
                start_line=token.line,
                start_col=token.col,
                end_line=token.line,
                end_col=token.col + len(val),
                value=val,
            )
        elif typ == "EMPTYLINE":
            self.pos += 1
            return EmptyLine(
                start_line=token.line,
                start_col=token.col,
                end_line=token.line,
                end_col=token.col + len(val),
            )
        else:
            raise SExprParseError(f"Unknown token type: {typ}", token)


class Formatter:
    def __init__(self, config: Config) -> None:
        self.config = config

    def indent_text(self, text: str, indent: int) -> str:
        """Prepend 'indent' spaces to each non-empty line in the text."""
        return "\n".join(
            " " * indent + line if line.strip() else "" for line in text.splitlines()
        )

    def split_block_form(
        self, elements: List[Node], header_count: int
    ) -> Tuple[List[Node], List[Node]]:
        """
        Split elements into header and body groups.
        Only non-comment and non-empty-line nodes count toward header_count,
        but comments and empty lines are preserved in order.
        """
        non_comment_count = 0
        until = 0
        for i, elem in enumerate(elements):
            if isinstance(elem, (Comment, EmptyLine)):
                continue
            non_comment_count += 1
            if non_comment_count == header_count:
                while i + 1 < len(elements) and isinstance(
                    elements[i + 1], (Comment, EmptyLine)
                ):
                    i += 1
                until = i + 1
                break
        else:
            until = len(elements)
        header_items = elements[:until]
        body_items = elements[until:]
        return header_items, body_items

    def _join_with_preserve_comment(self, elements: List[Node], indent: int) -> str:
        """
        Join elements with newlines, but no newline before same-line comments
        to preserve line structure like:

        ```
        ; Main entry point for running all tests
        (define (main)
            (if (run-example-test) ; keep this comment on the same line with run-example-test
                (exit 0)
                (exit 1)
            )
        )
        ```
        """
        if not elements:
            return ""

        cfg = self.config

        lines: List[str] = [self.format_node(elements[0], indent)]

        # if comment is on the same line as the previous element, merge them into the last line
        for prev, curr in zip(elements, elements[1:]):
            prev_line = prev.end_line
            curr_line = curr.start_line
            if isinstance(curr, Comment) and prev_line == curr_line:
                lines[-1] += " " + self.format_node(curr, 0).strip()
            else:
                lines.append(self.format_node(curr, indent))

        return "\n".join(lines)

    def format_node(self, node: Node, indent: int) -> str:
        cfg = self.config
        iw = cfg.indent_width
        inline_thr = cfg.inline_threshold
        block_forms = cfg.block_forms

        if isinstance(node, Atom):
            return (" " * indent + node.value) if indent > 0 else node.value
        elif isinstance(node, String):
            return " " * indent + node.value
        elif isinstance(node, Comment):
            return " " * indent + " " * cfg.space_before_comment + node.value
        elif isinstance(node, EmptyLine):
            return ""
        elif isinstance(node, QuoteNode):
            return " " * indent + "'" + self.format_node(node.expr, 0).strip()
        elif isinstance(node, ListNode):
            if node.elements:
                non_comment = [
                    e for e in node.elements if not isinstance(e, (Comment, EmptyLine))
                ]
                if (
                    non_comment
                    and isinstance(non_comment[0], Atom)
                    and non_comment[0].value in block_forms
                ):
                    header_count = block_forms[non_comment[0].value]
                    header_items, body_items = self.split_block_form(
                        node.elements, header_count
                    )
                    header_str = " ".join(
                        self.format_node(e, 0).strip() for e in header_items
                    )
                    header_line = " " * indent + f"({header_str}"
                    if body_items:
                        closing_line = " " * indent + ")"
                        body_str = self._join_with_preserve_comment(
                            body_items, indent + iw
                        )
                        return "\n".join([header_line, body_str, closing_line])
                    else:
                        return header_line + ")"
            if node.elements and not any(isinstance(e, Comment) for e in node.elements):
                inline = (
                    "("
                    + " ".join(self.format_node(e, 0).strip() for e in node.elements)
                    + ")"
                )
                if len(inline) <= inline_thr:
                    return " " * indent + inline

            inner_indent = indent + iw
            lines: List[str] = []
            if node.elements:
                first = self.format_node(node.elements[0], 0)
                lines.append(" " * indent + f"({first}")
                for e in node.elements[1:]:
                    lines.append(self.format_node(e, inner_indent))
                lines.append(" " * indent + ")")
                return "\n".join(lines)
            else:
                return " " * indent + "()"
        elif isinstance(node, LangDirective):
            return " " * indent + node.value
        else:
            raise SExprError("Unknown node type in format_node")

    def format_ast(self, ast_nodes: List[Node], indent: int = 0) -> str:
        return "\n".join(self.format_node(node, indent) for node in ast_nodes)

    def _find_split_index(self, line: str, max_len: int) -> Union[int, None]:
        if len(line) <= max_len:
            return None

        in_string_or_comment = False
        for i, c in enumerate(line):
            if c == '"':
                in_string_or_comment = not in_string_or_comment
            elif c == ";":
                in_string_or_comment = True

            if not in_string_or_comment and c.isspace():
                # Check if the current character is a space and the line length exceeds max_len
                if i > max_len:
                    return i

        return None

    # TODO: handle indent to make indentation consistent
    def apply_max_line_length(self, text: str) -> str:
        lines = text.splitlines()
        max_len = self.config.max_line_length
        formatted_lines: List[str] = []
        for line in lines:
            split_index = self._find_split_index(line, max_len)
            if split_index is None:
                # do not need or cannot split
                formatted_lines.append(line)
            else:
                # split the line at the found index
                first_part = line[:split_index]
                indent = " " * (len(line) - len(line.lstrip()))
                second_part = line[split_index:]
                formatted_lines.append(first_part)
                formatted_lines.append(indent + second_part)
        return "\n".join(formatted_lines)

    def normalize_newlines(self, text: str) -> str:
        return re.sub(r"\n{3,}", "\n\n", text)

    def canonicalize(self, s: str) -> str:
        parts = []
        in_string = False
        escape = False
        current = ""
        for char in s:
            if in_string:
                current += char
                if escape:
                    escape = False
                elif char == "\\":
                    escape = True
                elif char == '"':
                    in_string = False
                    parts.append(current)
                    current = ""
            else:
                if char == '"':
                    parts.append(re.sub(r"\s+", "", current))
                    current = '"'
                    in_string = True
                else:
                    current += char
        if current:
            if in_string:
                parts.append(current)
            else:
                parts.append(re.sub(r"\s+", "", current))
        return "".join(parts)

    def format(self, ast_nodes: List[Node]) -> str:
        formatted = self.format_ast(ast_nodes)
        formatted = self.apply_max_line_length(formatted)
        formatted = self.normalize_newlines(formatted)
        return formatted


def report_error(source: str, token: Token, message: str) -> None:
    """
    Report a parsing error in a Rust-like style:
    - Displays the error message in red bold text.
    - Shows the source line with the error.
    - Underlines the error position with a caret.
    """
    red = "\033[31m"
    bold = "\033[1m"
    reset = "\033[0m"
    lines = source.splitlines()
    if 1 <= token.line <= len(lines):
        error_line = lines[token.line - 1]
    else:
        error_line = ""
    caret = " " * (token.col - 1) + "^"
    print(f"{red}{bold}Error: {message} at line {token.line}, col {token.col}{reset}")
    print(error_line)
    print(f"{red}{caret}{reset}")


def debug_print_ast(node: Node, indent: int = 0) -> None:
    prefix = " " * indent
    if isinstance(node, Atom):
        print(f"{prefix}Atom: {node.value}")
    elif isinstance(node, String):
        print(f"{prefix}String: {node.value}")
    elif isinstance(node, Comment):
        print(f"{prefix}Comment: {node.value}")
    elif isinstance(node, EmptyLine):
        print(f"{prefix}EmptyLine")
    elif isinstance(node, QuoteNode):
        print(f"{prefix}QuoteNode:")
        debug_print_ast(node.expr, indent + 2)
    elif isinstance(node, ListNode):
        print(f"{prefix}ListNode:")
        for child in node.elements:
            debug_print_ast(child, indent + 2)
    else:
        print(f"{prefix}Unknown Node: {node}")


def main() -> None:
    source = sys.stdin.read()
    try:
        config = Config.load()
    except ConfigValidationError as e:
        print(e)
        sys.exit(1)
    tokenizer = Tokenizer(source)
    tokens = tokenizer.tokenize()
    tokens = filter_tokens(tokens)
    parser = Parser(tokens)
    try:
        ast_nodes = parser.parse()
    except SExprParseError as e:
        if hasattr(e, "token") and e.token is not None:
            report_error(source, e.token, str(e))
        else:
            print(f"Parsing error: {e}")
        sys.exit(1)
    formatter = Formatter(config)
    formatted = formatter.format(ast_nodes)
    print(formatted)


if __name__ == "__main__":
    main()
