# How to compile

Just clone the repository, then do : `cargo build`

# How does it work

In the idea of Lex/Yacc or Flex/Bison, this program takes a grammar files as an
argument and generate a lexical and a syntactic analyzer wich can be embedded in
a Rust program.

For now, it handles very simple grammar file which are written as shown below :

```
number : [0-9]+(.[0-9]+)?

S -> P '+' S
S -> P '-' S
S -> P
P -> F '*' P
P -> F '/' P
P -> F
F -> '(' S ')'
F -> number

```

**This is in heavy development and should not be used yet**
