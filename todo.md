
# compute FIRST(X)
first implement it for the base case, a single grammar asymbol

then implement it for a string of grammar symbols

# epsilon symbols in parsing
right now omitting them since grammar language doesn't allow them, but they might be needed for macros. for example `E?` might expand to:
```
E? =   E
     | ε
```
