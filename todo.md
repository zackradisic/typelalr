## epsilon symbols in parsing
right now omitting them since grammar language doesn't allow them, but they might be needed for macros. for example `E?` might expand to:
```
E? =   E
     | ε
```

## detecting ambiguity stackoverflow problem i think
This grammar causes stack overflow. I think it's because it causes an infinite loop bc of the Expr rule
```ts
export start Expr: ({ kind: "Expr", left: Expr, op: Op, right: Expr }) = [ 
     <l: Expr> <op: Op> <right: Expr> => ({ kind: "Expr", left: l, right: right, op: op })
]

export Op: ({ kind: "Op", op: "add" | "sub" | "div" | "mul" }) = [ 
     <plus: "+"> => ({ kind: "Op", op: "add" }),
     <sub: "-"> => ({ kind: "Op", op: "sub" }),
     <div: "/"> => ({ kind: "Op", op: "div" }),
     <mul: "*"> => ({ kind: "Op", op: "mul" }),
]

export Num: ({ kind: "Num", value: string }) = [
     <num: r"[1-9][0-9]*"> => ({ kind: "Num", value: num })
]
```