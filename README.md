# mini-C

A modular recursive descent parser for the C programming language written in **OCaml**. This project implements compiler architecture, including lexical analysis, parsing, abstract syntax tree (AST) generation, syntax validation, and type checking. The parser is designed with a modular architecture to make each compiler component independent, maintainable, and extensible.

---

## Features

- Recursive descent parser implemented entirely in OCaml
- Modular compiler architecture
- Lexical analysis (tokenization)
- Syntax parsing for a subset of the C language
- Abstract Syntax Tree (AST) generation
- Easily extensible grammar and parser rules
- Designed for compiler and programming language research

---

## Project Overview

This project implemented every grammar rule as an OCaml function using the recursive descent parsing technique.

The parser currently reads C source code, tokenizes the input, validates its syntax according to the implemented grammar, and constructs an Abstract Syntax Tree representing the program structure.

---
