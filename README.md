# How to compile

Just clone the repository, then do : `cargo build`

# How does it work

In the idea of Lex/Yacc or Flex/Bison, this program takes a grammar files as an
argument and generate a lexical and a syntactic analyzer wich can be embedded in
a Rust program.

For now, it handles very simple grammar file which are written as shown below :

```
number : [0-9]+(\.[0-9]+)?

Expression -> Product '\+' Expression
Expression -> Product '-' Expression
Expression -> Product
Product -> Factor '\*' Product
Product -> Factor '/' Product
Product -> Factor
Factor -> '\(' Expression '\)'
Factor -> number

```

# Running the tests

**Note :** Test programms are also examples you can base your work on.

_Example for the 'expr' test_
1. Generate the parser code `cargo run test/expr/src/expr.lalr`
2. `cd test/expr`
3. Compile the test `cargo build`
4. Run it `cargo run`

**This is in heavy development and should not be used yet**

**Feedback are always welcomed :)**
