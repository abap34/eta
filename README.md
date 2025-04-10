# eta

**eta** is a toy Scheme interpreter written in Scheme. 

- [ ] Tail Call Optimization (TCO)
- [ ] First-class Continuations (`call/cc`)
- [ ] Hygienic Macros


---

## 📦 Project Structure

```
eta/
├── eta/           ; Main source code
├── tests/         ; Unit tests
├── examples/      ; Example programs
├── tools/         ; Utility scripts
├── Makefile       ; Build and test 
└── main.scm       ; Main entry point
```

---

## 🚀 Getting Started

### Requirements

Scheme implementation that supports R6RS.
I built and tested with: [Chez Scheme](https://cisco.github.io/ChezScheme/) 

### Installation

```
$ make install 
$ make install PREFIX=/usr/local SCHEME=chezscheme BINDIR=/usr/local/bin  # optional. see Makefile for more options
```

### Enter the REPL

```
$ eta
```

### Run a file

```
$ eta <filename>
```

### Run tests

```
$ make test
```


## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

