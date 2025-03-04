# FORTH Implementation in Haskell

This project implements a simple FORTH interpreter in Haskell. The interpreter supports basic FORTH operations, string manipulation, and user-defined functions.

## Overview

This FORTH implementation provides:
- Core stack manipulation operations (DUP, DROP, SWAP, OVER, ROT)
- Basic arithmetic operations (+, -, *, /, ^)
- String handling (CONCAT2, CONCAT3)
- Output operations (., EMIT, CR)
- Type conversion (STR)
- User-defined functions with the `: name ... ;` syntax (Bonus)


## Building and Running

### Prerequisites
- GHC (Glasgow Haskell Compiler)
- Cabal (Haskell build system)

### Installing packages
```bash
cabal install
cabal install hbase
```

### Clean Instructions
```bash
cabal clean
```

### Build Instructions
```bash
cabal build
```

### Running the Interpreter
```bash
cabal run FORTH -- [filename]>[outputfilename]
```

Where `[filename]` is the path to a FORTH program test file and `[outputfilename]` is the path to store the test file output. If no file is specified, the interpreter will run in interactive mode.

## Project Structure

The project is organized as follows:

```bash
FORTH/
├── app/
│   └── Main.hs          # Entry point for the interpreter
├── src/
│   ├── Eval.hs          # Core evaluation logic
│   ├── Val.hs           # Value type definitions
│   ├── Interpret.hs     # Tokenization and parsing
│   └── Setup.hs         # Build system setup
├── test/
│   ├── EvalSpec.hs      # Tests for evaluation logic
│   ├── InterpretSpec.hs # Tests for tokenization and parsing
│   └── ValSpec.hs       # Tests for value type definitions
├── tests/               # Test FORTH programs
│   ├── t1.4TH
│   ├── t2.4TH
│   └── ...
│   └── t10.4TH
│   └── t11.4TH          # Bonus test cases
│   └── ...
│   └── t15.4TH
├── FORTH.cabal          # Cabal build configuration
└── README.md            # This file
```

Here's the corrected Key Components section for your README:

### Key Components

- **Eval.hs**: Contains the main evaluation logic, including the implementation of all FORTH operations and the function definition mechanism.
- **Val.hs**: Defines the value types used by the interpreter (IntVal, FloatVal, StringVal).
- **Interpret.hs**: Handles tokenization and parsing of FORTH programs.
- **Setup.hs**: Contains build system setup for Cabal.
- **Main.hs**: Provides the interpreter interface, handling file input/output and interactive mode.
- **test/EvalSpec.hs**: Contains unit tests for the evaluation logic.
- **test/InterpretSpec.hs**: Contains unit tests for tokenization and parsing.
- **test/ValSpec.hs**: Contains unit tests for value type definitions.
- **tests/**: Contains FORTH program files (*.4TH) for testing the interpreter, including standard and bonus test cases.

## Supported Operations

### Stack Manipulation
- `DUP` - Duplicate the top stack item
- `DROP` - Remove the top stack item
- `SWAP` - Swap the top two stack items
- `OVER` - Copy the second stack item to the top
- `ROT` - Rotate the top three stack items

### Arithmetic
- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `/` - Division
- `^` - Exponentiation

### String Operations
- `CONCAT2` - Concatenate two strings
- `CONCAT3` - Concatenate three strings
- `STR` - Convert a value to a string

### Output
- `.` - Print and remove the top stack item
- `EMIT` - Output the character corresponding to an ASCII code
- `CR` - Print a newline

### Function Definition (Bonus)
- `: WORD ... ;` - Define a new word (function)

## Error Handling

The interpreter provides error messages for common issues:
- Stack underflow
- Division by zero
- Invalid operations
- Unknown tokens

## Limitations

- Limited error reporting
- Limited type system
- No file I/O operations
