# Improvement directions

## Language features

- Support for closures and heap-allocated variables
- Support for interfaces that modules may implement
- Make `panic` an expression of type `Nothing` rather than a statement
- Support for basic expressions in constants initializers


## Compiler improvements

- Make lowering phase less aggressive (this produces overcomplicated bytecode)
- Add an optimizer
- Inline constants at compile-time instead of storing them in a separate class


