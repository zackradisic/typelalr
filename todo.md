## epsilon symbols in parsing
right now omitting them since grammar language doesn't allow them, but they might be needed for macros. for example `E?` might expand to:
```
E? =   E
     | ε
```

## code generation!
generate pretty much everything in `parse_state.gen.ts`:
* Symbol (production outputs)
* Token (inputs for production functions)
* Production (just a regular declaration)
* Tokens 
* Productions
* Action
* Goto
* TokenKindToIdx
* actions module
  * variants of Symbol
  * action impls (they are light-wrappers over user defined production fns)
  * actions (they do the popping of the symbol stack and all that other BS)
  * EmitProduction (just dispatching the actions)