import { type_defs, actions } from "./parse_state2.gen";
import * as U from "./util";
import * as L from "./lex";

type GetAction<
  top extends any,
  a extends number
> = top extends keyof type_defs.Action
  ? a extends keyof type_defs.Action[top]
    ? type_defs.Action[top][a]
    : "error: `a` is not a key in `Action[top]`"
  : "error: `top` is not a key in `Action`";

type GetGoto<
  stateIdx extends number,
  productionName extends string
> = stateIdx extends keyof type_defs.Goto
  ? productionName extends keyof type_defs.Goto[stateIdx]
    ? type_defs.Goto[stateIdx][productionName]
    : "error: `productionName` is not a key in `Goto[stateIdx]`"
  : `error: \`stateIdx\` is not a key in \`Goto\` (stateIdx=${stateIdx})`;

type NextToken<
  a extends number,
  i extends number,
  input extends number[]
> = U.Succ<i> extends infer iPlusOne
  ? iPlusOne extends number
    ? U.Lt<iPlusOne, input["length"]> extends true
      ? iPlusOne extends keyof input
        ? [input[iPlusOne], iPlusOne]
        : `error: input idx out of range (${iPlusOne}>=${input["length"]})`
      : [a, i]
    : `error: incrementing \`i\` result is not a number (i=${i})`
  : `error: incrementing \`i\` (${i})`;

type TokenIndices<toks extends L.Token[]> = TokenIndicesImpl<
  toks,
  []
> extends infer ret
  ? ret extends number[]
    ? U.Reverse<ret>
    : ret
  : "unreachable: inferring `ret` never fails";
type TokenIndicesImpl<
  toks extends L.Token[],
  ret extends number[]
> = toks extends [infer head, ...infer tail]
  ? head extends L.Token
    ? head["kind"] extends keyof type_defs.TokenKindToIdx
      ? tail extends L.Token[]
        ? TokenIndicesImpl<
            tail,
            [type_defs.TokenKindToIdx[head["kind"]], ...ret]
          >
        : "unreachable: tail should always be L.Token[]"
      : `error: head['kind'] is not a valid token kind: ${head["kind"]}`
    : "error: `head` is not a lexer token"
  : ret;

type ParseImplShift<
  newStateIdx extends number,
  tokenInputs extends L.Token[],
  input extends number[],
  stack extends number[],
  i extends number,
  a extends number,
  symbolStack extends type_defs.Symbol[],
  maxRecursionDepth extends number
> = NextToken<a, i, input> extends infer nextTok extends [
  newA: number,
  newI: number
]
  ? ParseImpl<
      tokenInputs,
      input,
      [newStateIdx, ...stack],
      nextTok[1],
      nextTok[0],
      [
        { kind: "token"; token: tokenInputs[i]; lmao: newStateIdx },
        ...symbolStack
      ],
      U.Pred<maxRecursionDepth>
    >
  : "error: todo fill this out"; /* NextToken<a, i, input>*/ // else it's an error so return it

type ParseImplReduce<
  ruleIdx extends number,
  production extends type_defs.Production,
  tokenInputs extends L.Token[],
  input extends number[],
  stack extends number[],
  i extends number,
  a extends number,
  symbolStack extends type_defs.Symbol[],
  maxRecursionDepth extends number
> = U.PopNStack<stack, production["tokens"]["length"]> extends infer newStack
  ? newStack extends [number, ...infer newRestOfStack]
    ? GetGoto<
        newStack[0],
        production["name"]
      > extends infer goto_ta extends number
      ? newRestOfStack extends number[]
        ? actions.EmitProduction<
            ruleIdx,
            symbolStack
          > extends infer newSymbolStack extends type_defs.Symbol[]
          ? ParseImpl<
              tokenInputs,
              input,
              [goto_ta, newStack[0], ...newRestOfStack],
              i,
              a,
              newSymbolStack,
              U.Pred<maxRecursionDepth>
            >
          : [
              `error state=${stack[0]} a=${a}`,
              actions.EmitProduction<ruleIdx, symbolStack>,
              symbolStack
            ] //actions.EmitProduction<ruleIdx, symbolStack> //symbolStack // 'error' // [stack, newSymbolStack]
        : "unreachable: `newRestOfStack` should always be a number[]"
      : "not good" // goto_ta // else it's an error message so return it
    : "unreachable: `newStack` is always a number"
  : "unreachable: inferring `newStack` never fails";

type ParseImpl<
  tokenInputs extends L.Token[],
  input extends number[],
  stack extends number[],
  i extends number,
  a extends number,
  symbolStack extends type_defs.Symbol[],
  maxRecursionDepth extends number
> = maxRecursionDepth extends 0
  ? "exceeded max recursion depth"
  : stack extends [infer top, ...infer _restOfStack]
  ? GetAction<top, a> extends infer action
    ? action extends { shift: number }
      ? ParseImplShift<
          action["shift"],
          tokenInputs,
          input,
          stack,
          i,
          a,
          symbolStack,
          maxRecursionDepth
        >
      : action extends { reduce: infer ruleIdx }
      ? ruleIdx extends keyof type_defs.Productions
        ? type_defs.Productions[ruleIdx] extends infer production extends type_defs.Production
          ? ParseImplReduce<
              ruleIdx,
              production,
              tokenInputs,
              input,
              stack,
              i,
              a,
              symbolStack,
              maxRecursionDepth
            >
          : "unreachable: `Productions[ruleIdx]` is not a production"
        : "error: `ruleIdx` is not a key in `Productions`"
      : action extends { accept: true }
      ? symbolStack //[symbolStack, stack] // accept
      : [`error: invalid token ${a}`, symbolStack]
    : "unreachable: inferring `action` should never fail"
  : "error: stack is empty";

type Parse<tokenInputs extends L.Token[]> =
  TokenIndices<tokenInputs> extends infer input
    ? input extends number[]
      ? ParseImpl<tokenInputs, input, [0], 0, input[0], [], 50>
      : input extends string
      ? input
      : "error: `input` should always be a number[] or string"
    : "unreachable: inferring `input` never fails";

type tokens = L.Lex<"(add 420 420 420 420)">;
type wtf = Parse<tokens>;

type bruh = wtf[0]["value"]["exprs"][0];
type lol = wtf[0]["value"]["exprs"][0]["kind"]; // extends { kind: "symbol", value: "add"} ? "NICE" : "NOT NICE"

// type arr = [{
//   kind: "Exprs";
//   value: [{
//       kind: "symbol";
//       value: "add";
//   }];
// }, {
//   kind: "Expr";
//   value: {
//       kind: "symbol";
//       value: "add";
//   };
// }, {
//   kind: "token";
//   token: {
//       kind: "(";
//       value: "(";
//   };
//   lmao: 2;
// }]

// type tokenIndices = TokenIndices<tokens>;
// type help = ParseImpl<tokens, TokenIndices<tokens>, [0], 0, tokenIndices[0], [], 50>
// type help = ParseImpl<
//   tokens,
//   TokenIndices<tokens>,
//   [11, 6, 2, 0],
//   7,
//   0,
//   [
//     {
//       kind: "token";
//       token: { kind: ")"; value: ")" };
//     },
//     // { kind: "Expr"; value: { kind: "int"; value: "420" } },
//     {
//       kind: "Exprs",
//       value: [
//         { kind: "sexpr", sexpr: { kind: "SExpr", exprs: [ { kind: "symbol", value: "add" }, { kind: "symbol", value: "add" } ] }},
//         { kind: "symbol", value: "add" }
//       ]
//     },
//     {
//       kind: "token";
//       token: { kind: "("; value: "(" };
//     }
//   ],
//   50
// >;

type tokenIndices = TokenIndices<tokens>;
// type help = ParseImpl<tokens, TokenIndices<tokens>, [0], 0, tokenIndices[0], [], 50>
type help = ParseImpl<
  tokens,
  TokenIndices<tokens>,
  [11, 6, 2, 0],
  7,
  0,
  [
    {
      kind: "token";
      token: { kind: ")"; value: ")" };
    },
    // { kind: "Expr"; value: { kind: "int"; value: "420" } },
    {
      kind: "Exprs";
      value: [
        {
          kind: "sexpr";
          sexpr: {
            kind: "SExpr";
            exprs: [
              { kind: "symbol"; value: "add" },
              { kind: "symbol"; value: "add" }
            ];
          };
        },
        { kind: "symbol"; value: "add" }
      ];
    },
    {
      kind: "token";
      token: { kind: "("; value: "(" };
    }
  ],
  50
>;
