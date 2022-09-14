# PL0-Compiler

![standard-readme compliant](https://img.shields.io/github/license/SugarSBN/PL0-Compiler)![](https://img.shields.io/badge/language-Haskell-brightgreen)![](https://img.shields.io/badge/base-^>=4.14.3.0-brightgreen)

A simple compiler for the toy language PL/0, implemented by Haskell.

- [Feature](#feature)
- [Usage](#usage)
- [License](#license)

## Feature

* Monadic Parsing
* interpreter for three address code

## Usage

```
cabal build
cabal new-run PL0-Compiler -- ./testfiles/PL0_code_gcd.pas
```

## License

[MIT © Richard McRichface.](../LICENSE)