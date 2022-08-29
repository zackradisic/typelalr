## **typelalr**
Type-level Lexer/parser generators. Aka a really bad idea.

## How does it work?
For the lexer/tokenizer, Rust generates a type-level finite state machine (a DFA specifically) that tokenizes an input string for you.

For the parser, Rust generates a type-level LALR 

## Limitations
### Character set is limited
Only the first 256 unicode characters are supported, this is because we have to use a char table to support char ranges

### Input length
Only inputs 5000 characters or less will work because we use arithmetic operations to substring the input string for capturing token values.
This is because a successor/predecessor table limited to the [0-5000] range is used to faciliate arithmetic operations.