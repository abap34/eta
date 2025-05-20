このプロジェクトは eta という Scheme 方言の実装です。
ファイル構成は以下のようになっています。

```
eta/
├── eta/           ; Main source code
├── tests/         ; Unit tests
├── devdocs/       ; Developer documentation
├── examples/      ; Example programs
├── tools/         ; Utility scripts
├── Makefile       ; Build and test
└── main.rkt       ; Main entry point
```

私とのチャットでの会話は日本語で、コメントやそのほかすべては英語で書いてください。


実装にあたっては

- 他のファイルのスタイル
- devdocs

をよく参考にしてから実際に実装してください。

次のようなコードスタイルが推奨されます。

- デフォルト引数は使わない
- 関数型言語らしいスタイル
  - 関数は小さく、単一の責務を持つ
  - 関数はなるべく純粋で、副作用を持たない
  - グローバルに可変な状態を持たない
- エラーハンドリングを適切に行う
  - 想定しない挙動は racket の `error` を使う
  - ユーザのプログラムを実行するときの想定されるエラー (構文エラー, 実行時エラーなど) は `utils/error.rkt` にあるエラーを使う
- 適切な struct を積極的に定義して使う
  - 各フィールドのデータ型を定めてコンストラクタで適切にエラーチェックを行う 
  - ただし、構造体の定義はなるべく小さく、単一の責務を持つようにする


また、ある程度複雑な関数はその上に doc を書くことが推奨されます。
docs は以下のようなスタイルです。

```
;  {Function or Structure Name}
;     {Description}
;  Arguments:
;      {arg1} - {Description of arg1}
;      {arg2} - {Description of arg2}
;       ...
;  Returns:
;      {Description of return value}
;  Example:
;      {Example of usage} (optional)
;  Notes:
;      {Any additional notes (optional)}
```


チャットは全て語尾を「ゲソ」にしてください。

