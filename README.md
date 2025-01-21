
## The Grattlesnake programming language

For the original version of the Rattlesnake language, see the `rattlesnake-v0.1` branch.

This branch contains an experimental implementation of the Gradient type system (Boruch-Gruszecki et al., 2024) inside the Rattlesnake language (hence the Grattlesnake name! 🐍).

This work is part of my Master's thesis (EPFL SYSTEMF lab, fall semester 2024-2025).


## Example program

An example program (a sudoku solver) can be found at https://github.com/ValentinAebi/sudoku-case-study.


## Language documentation

A [language documentation](./grattlesnake-lang-doc.md) is available in a separate document.


## Repository structure

This repository contains folders that correspond to the three main modules of the Grattlesnake language:
- [The compiler](./rattlesnake-compiler): compiles Grattlesnake programs to Java bytecode.
- [The runtime](./rattlesnake-runtime): classes that get injected in Grattlesnake program. Responsible for devices, environments, and runtime checks.
- [The agent](./rattlesnake-agent): a Java agent that collaborates with the runtime classes to perform runtime checks on object mutations.

Documentation about the compiler phases can be found [here](./rattlesnake-compiler/compiler-doc.md).

The tests for the whole system are located in the compiler directory. Documentation describing the testing system can be found [here](./rattlesnake-compiler/tests-infrastructure-doc.md).

## Build and dependencies

The following dependencies are required to build the system:
- A Java Development Kit (JDK). Java 21 or above is required. Version used during development: [Java 22.0.2](https://www.oracle.com/java/technologies/javase/jdk22-archive-downloads.html).
- Scala 3, which can be downloaded from [here](https://www.scala-lang.org/download/). Version used during development: 3.5.0
- [SBT](https://www.scala-sbt.org/download/) to build the compiler. Version used during development: 1.10.2.
- [Maven](https://maven.apache.org/download.cgi) to build the runtime and the agent. Version used during development: 3.8.1.

Python 3 is required to run the main build script, which makes things substantially easier. Python 3.12.0 has been used during development.
You may also want to run the local Makefile scripts, in which case you will need to have make installed on your machine. Installation instructions: [Ubuntu](https://linuxhint.com/install-make-ubuntu/), [Windows](https://www.technewstoday.com/install-and-use-make-in-windows/)

To build the compiler, runtime, and agent at once, go to the [mkjars directory](./mkjars-script) and run `python mk_jars.py`. If you want to skip compilation of the compiler (which takes a bit of time), run `python mk_jars.py skip-compiler`. This script creates a new directory named `jars` and writes the jars corresponding to the runtime, agent, and compiler (if not skipped) in it.


## Compiling and running Grattlesnake programs

1. Copy the jars (compiler, runtime, and agent, which can be found in the `jars` directory) into your project root folder.
2. Create a `src` directory in your project root folder. Write your code in files with the `.rsn` extension, inside this directory (subdirectories are not supported).
3. Run the following command: `java -jar rattlesnake-compiler-0.2.0-snapshot.jar run src/*.rsn`

You may want to refer to the example project mentioned above (the commands can be found in the Makefile of this project).

The compiler offers other commands. Just run `java -jar rattlesnake-compiler-0.2.0-snapshot.jar help` for a list of supported commands. Note that some of these commands may not be very stable.


## Todo list

A [list of improvement directions](./improvement-directions.md) is available in a separate document.


## Library references

The lexer and the parser are inspired from https://github.com/epfl-lara/silex and https://github.com/epfl-lara/scallion, respectively (but they do not use these libraries).

The backend and the agent use the ASM bytecode manipulation library for code generation: https://asm.ow2.io/

The runtime uses the fastutil type-specific collection framework: https://fastutil.di.unimi.it/

