export declare module type_defs {
  export type Symbol =
    | { kind: "token"; token: L.Token }
    | { kind: "SExpr"; value: actions.SExpr }
    | { kind: "Exprs"; value: actions.Exprs }
    | { kind: "Expr"; value: actions.Expr };

  export type Token =
    | { kind: "strlit"; value: string }
    | { kind: "nonterminal"; value: string }
    | { kind: "regex"; value: string }
    | { kind: "eof" };

  export type Tokens = {
    0: { kind: "eof" };
    1: { kind: "strlit"; value: "(" };
    2: { kind: "strlit"; value: ")" };
    3: { kind: "strlit"; value: "add" };
    4: { kind: "nonterminal"; value: "Expr" };
    5: { kind: "nonterminal"; value: "Exprs" };
    6: { kind: "nonterminal"; value: "SExpr" };
    7: { kind: "regex"; value: "[1-9]+[0-9]*" };
  };

  export type Production = {
    name: string;
    tokens: Array<keyof Tokens>;
  };

  export type Productions = {
    0: { name: "SExpr'"; tokens: [6] };
    1: { name: "SExpr"; tokens: [1, 5, 2] };
    2: { name: "Exprs"; tokens: [4, 5] };
    3: { name: "Exprs"; tokens: [4] };
    4: { name: "Expr"; tokens: [6] };
    5: { name: "Expr"; tokens: [3] };
    6: { name: "Expr"; tokens: [7] };
  };

  export type Action = {
    0: { 1: { shift: 2 } };
    1: { 0: { accept: true } };
    2: { 1: { shift: 2 }; 3: { shift: 7 }; 7: { shift: 3 } };
    3: {
      1: { reduce: 6 };
      2: { reduce: 6 };
      3: { reduce: 6 };
      7: { reduce: 6 };
    };
    4: {
      1: { reduce: 4 };
      2: { reduce: 4 };
      3: { reduce: 4 };
      7: { reduce: 4 };
    };
    5: { 2: { shift: 9 } };
    6: { 1: { shift: 2 }; 2: { reduce: 3 }; 3: { shift: 7 }; 7: { shift: 3 } };
    7: {
      1: { reduce: 5 };
      2: { reduce: 5 };
      3: { reduce: 5 };
      7: { reduce: 5 };
    };
    8: { 2: { reduce: 2 } };
    9: {
      0: { reduce: 1 };
      1: { reduce: 1 };
      2: { reduce: 1 };
      3: { reduce: 1 };
      7: { reduce: 1 };
    };
  };
  export type Goto = {
    0: {
      SExpr: 1;
    };
    2: {
      Expr: 6;

      Exprs: 5;

      SExpr: 4;
    };
    6: {
      Expr: 6;

      Exprs: 8;

      SExpr: 4;
    };
  };
  export type TokenKindToIdx = {
    EOF: 0;
    "(": 1;
    ")": 2;
    add: 3;
    "[1-9]+[0-9]*": 7;
  };
}
import * as U from "./util";
import * as L from "./lex";

export declare module actions {
  type Expr =
    | { kind: "symbol"; value: "add" }
    | { kind: "int"; value: string }
    | { kind: "sexpr"; sexpr: SExpr };
  type Exprs = Expr[];
  type SExpr = { kind: "SExpr"; exprs: Exprs };

  type EmitProduction<
    prodIdx extends number,
    symbolStack extends type_defs.Symbol[]
  > = prodIdx extends 1
    ? actions.Action1<symbolStack>
    : prodIdx extends 2
    ? actions.Action2<symbolStack>
    : prodIdx extends 3
    ? actions.Action3<symbolStack>
    : prodIdx extends 4
    ? actions.Action4<symbolStack>
    : prodIdx extends 5
    ? actions.Action5<symbolStack>
    : prodIdx extends 6
    ? actions.Action6<symbolStack>
    : `error: invalid production idx: ${prodIdx}`;

  type Action1<symbolStack extends type_defs.Symbol[]> = U.PopNStackWithValue<
    symbolStack,
    3
  > extends [infer newStack, infer poppedValues]
    ? poppedValues extends [infer var0, infer __exprs, infer var1]
      ? newStack extends type_defs.Symbol[]
        ? __exprs extends { kind: "Exprs"; value: infer exprs extends Exprs }
          ? [
              { kind: "SExpr"; value: { kind: "SExpr"; exprs: exprs } },
              ...newStack
            ]
          : "error: invalid symbol (pos `2`, Action1)"
        : `unreachable: \`newStack\` should always be a type_defs.Symbol[] (Action1)`
      : `error: popping off symbol Stack Action1`
    : "unreachable: inferring popped values never fails";
  type Action2<symbolStack extends type_defs.Symbol[]> = U.PopNStackWithValue<
    symbolStack,
    2
  > extends [infer newStack, infer poppedValues]
    ? poppedValues extends [infer __eee, infer __es]
      ? newStack extends type_defs.Symbol[]
        ? __eee extends { kind: "Expr"; value: infer eee extends Expr }
          ? __es extends { kind: "Exprs"; value: infer es extends Exprs }
            ? [{ kind: "Exprs"; value: [eee, ...es] }, ...newStack]
            : "error: invalid symbol (pos `1`, Action2)"
          : "error: invalid symbol (pos `2`, Action2)"
        : `unreachable: \`newStack\` should always be a type_defs.Symbol[] (Action2)`
      : `error: popping off symbol Stack Action2`
    : "unreachable: inferring popped values never fails";
  type Action3<symbolStack extends type_defs.Symbol[]> = U.PopNStackWithValue<
    symbolStack,
    1
  > extends [infer newStack, infer poppedValues]
    ? poppedValues extends [infer __eee]
      ? newStack extends type_defs.Symbol[]
        ? __eee extends { kind: "Expr"; value: infer eee extends Expr }
          ? [{ kind: "Exprs"; value: [eee] }, ...newStack]
          : "error: invalid symbol (pos `1`, Action3)"
        : `unreachable: \`newStack\` should always be a type_defs.Symbol[] (Action3)`
      : `error: popping off symbol Stack Action3`
    : "unreachable: inferring popped values never fails";
  type Action4<symbolStack extends type_defs.Symbol[]> = U.PopNStackWithValue<
    symbolStack,
    1
  > extends [infer newStack, infer poppedValues]
    ? poppedValues extends [infer __sexpr]
      ? newStack extends type_defs.Symbol[]
        ? __sexpr extends { kind: "SExpr"; value: infer sexpr extends SExpr }
          ? [
              { kind: "Expr"; value: { kind: "sexpr"; sexpr: sexpr } },
              ...newStack
            ]
          : "error: invalid symbol (pos `1`, Action4)"
        : `unreachable: \`newStack\` should always be a type_defs.Symbol[] (Action4)`
      : `error: popping off symbol Stack Action4`
    : "unreachable: inferring popped values never fails";
  type Action5<symbolStack extends type_defs.Symbol[]> = U.PopNStackWithValue<
    symbolStack,
    1
  > extends [infer newStack, infer poppedValues]
    ? poppedValues extends [infer __str]
      ? newStack extends type_defs.Symbol[]
        ? __str extends { kind: "token"; token: L.Token }
          ? __str["token"]["value"] extends infer str extends "add"
            ? [
                { kind: "Expr"; value: { kind: "symbol"; value: str } },
                ...newStack
              ]
            : `error: not a token (pos \`1\`, Action5)`
          : `error: invalid symbol (pos \`1\`, Action5)`
        : `unreachable: \`newStack\` should always be a type_defs.Symbol[] (Action5)`
      : `error: popping off symbol Stack Action5`
    : "unreachable: inferring popped values never fails";
  type Action6<symbolStack extends type_defs.Symbol[]> = U.PopNStackWithValue<
    symbolStack,
    1
  > extends [infer newStack, infer poppedValues]
    ? poppedValues extends [infer __int]
      ? newStack extends type_defs.Symbol[]
        ? __int extends { kind: "token"; token: L.Token }
          ? __int["token"]["value"] extends infer int extends string
            ? [
                { kind: "Expr"; value: { kind: "int"; value: int } },
                ...newStack
              ]
            : `error: not a token (pos \`1\`, Action6)`
          : `error: invalid symbol (pos \`1\`, Action6)`
        : `unreachable: \`newStack\` should always be a type_defs.Symbol[] (Action6)`
      : `error: popping off symbol Stack Action6`
    : "unreachable: inferring popped values never fails";
}
