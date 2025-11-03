Program 2 — Scanner & Parser (Racket)
Overview

This project implements a scanner and parser in Racket for a small teaching language.

The scanner reads an input file and produces a stream of tokens with line/column tracking.

The parser validates syntax and builds a structured tree that’s printed in a readable, pretty parse-tree/AST format (not a raw/unformatted dump).

Supported statements include READ, PRINT, assignments, and IF … THEN … BEGIN … END blocks, with nesting and standard expression precedence.

Prerequisites

Racket (latest stable)

Optional editor setup:

VS Code with the Racket extension (recommended)

OR DrRacket

How to Run
A) VS Code (Racket extension)

Open the project folder in VS Code.

Open src/main.rkt.

Set the input file path inside main.rkt to the test file you want to run (e.g., a file under tests/valid or tests/invalid).

Run the file via the Racket extension (Run/Execute command).

B) Command Line

From the project root:

racket src/main.rkt


Tip: You can also run from inside src/ with:

racket main.rkt


The program will read the chosen test file, run scanning/parsing, print a pretty parse tree/AST, and report any syntax errors with line/column locations.

Features

Tokenization with positions: lexemes include line and column for precise diagnostics.

Meaningful syntax errors: clear messages pointing to where parsing failed.

Readable output: prints a pretty parse tree/AST that’s easy to inspect.

Tests included: sample inputs under tests/valid and tests/invalid.

Note on print/debug statements:
Some debug printfs remain in the source (for troubleshooting) but are not used during normal runs. They were helpful while building the scanner and have been left in place but are disabled/by-passed in the current flow.

Project Structure
Program2ScannerParserJW/
│
├─ src/
│  ├─ main.rkt     # Entry point — wires scanner & parser and selects input file
│  ├─ scanner.rkt  # Lexical analyzer (tokenizer)
│  └─ parser.rkt   # Syntax analyzer / pretty parse-tree/AST builder
│
├─ tests/
│  ├─ valid/       # Valid input files
│  └─ invalid/     # Intentionally broken inputs for error handling
│
└─ README.md

Error Handling

The scanner and parser both surface clear, line/column-aware errors.

Invalid cases (e.g., missing END, malformed numbers, bad operators) should appear in tests/invalid/ and produce helpful diagnostics.

Version Control & AI Assistance

Frequent, descriptive commits throughout development on GitHub.

Source comments mark where AI assistance informed changes (ChatGPT / GitHub Copilot / Claude). All final logic and tests were reviewed and validated by the author.

Legacy debug prints were retained for future troubleshooting but are not part of normal output.

Author & Course

JoshaLynn Worth
University of Missouri–Kansas City (UMKC)
COMP_SCI 431 – Programming Languages / Operating Systems integration project

Acknowledgments: ChatGPT, GitHub Copilot, and Claude were used as coding co-pilots for brainstorming, refactors, and documentation polish; final decisions, testing, and submissions are the author’s.
