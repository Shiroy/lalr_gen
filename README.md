# How to compile

Just clone the repository, then do : `cargo build`

# How does it work

In the idea of Lex/Yacc or Flex/Bison, this program takes a grammar files as an
argument and generate a lexical and a syntactic analyzer wich can be embedded in
a Rust program.

For now, it handles very simple grammar file which are written as shown below :

```
number : [0-9]+(.[0-9]+)?

Expression -> Product '\+' Expression
Expression -> Product '-' Expression
Expression -> Product
Product -> Factor '\*' Product
Product -> Factor '/' Product
Product -> Factor
Factor -> '\(' Expression '\)'
Factor -> number

```

**This is in heavy development and should not be used yet**
