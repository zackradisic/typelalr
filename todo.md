## epsilon symbols in parsing
right now omitting them since grammar language doesn't allow them, but they might be needed for macros. for example `E?` might expand to:
```
E? =   E
     | ε
```

## store token values for outputting productions
we need to store the values of tokens we shift so 
they can be used when outputting productions