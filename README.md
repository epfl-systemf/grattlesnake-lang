
## The Rattlesnake programming language, Gradient version

This branch contains an experimental implementation of the Gradient type system (Boruch-Gruszecki et al., 2024) inside the Rattlesnake language.

This work is part of my Master's thesis (EPFL SYSTEMF lab, fall semester 2024-2025).

Documentation may not be up to date. For documentation about the original version of the Rattlesnake language, see the `rattlesnake-v0.1` branch.


## References

The lexer and the parser are inspired from https://github.com/epfl-lara/silex and https://github.com/epfl-lara/scallion, respectively.

The backend and the agent use the ASM bytecode manipulation library for code generation: https://asm.ow2.io/

The runtime uses the fastutil type-specific collection framework: https://fastutil.di.unimi.it/

