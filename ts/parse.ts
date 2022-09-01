import {
  Action,
  Goto,
  Production,
  Productions,
  Symbol,
  actions,
  TokenKindToIdx,
} from "./parse_state.gen";
import * as U from "./util";
import * as L from "./lex";

type GetAction<top extends any, a extends number> = top extends keyof Action
  ? a extends keyof Action[top]
    ? Action[top][a]
    : "error: `a` is not a key in `Action[top]`"
  : "error: `top` is not a key in `Action`";

type GetGoto<
  stateIdx extends number,
  productionName extends string
> = stateIdx extends keyof Goto
  ? productionName extends keyof Goto[stateIdx]
    ? Goto[stateIdx][productionName]
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
    ? head["kind"] extends keyof TokenKindToIdx
      ? tail extends L.Token[]
        ? TokenIndicesImpl<tail, [TokenKindToIdx[head["kind"]], ...ret]>
        : "unreachable: tail should always be L.Token[]"
      : `error: head['kind'] is not a valid token kind: ${head["kind"]}`
    : "error: `head` is not a lexer token"
  : ret;

type ParseImpl<
  tokenInputs extends L.Token[],
  input extends number[],
  stack extends number[],
  i extends number,
  a extends number,
  symbolStack extends Symbol[]
> = stack extends [infer top, ...infer _restOfStack]
  ? GetAction<top, a> extends infer action
    ? action extends { shift: infer newStateIdx }
      ? newStateIdx extends number
        ? NextToken<a, i, input> extends [infer newA, infer newI]
          ? newI extends number
            ? newA extends number
              ? ParseImpl<
                  tokenInputs,
                  input,
                  [newStateIdx, ...stack],
                  newI,
                  newA,
                  [{ kind: "token"; token: tokenInputs[i] }, ...symbolStack]
                >
              : `unreachable: \`newA\` should be a number here (${newI})`
            : "unreachable: `newI` should be a number here"
          : "error: todo fill this out" /* NextToken<a, i, input>*/ // else it's an error so return it
        : "error: `newStateIdx` is not a number"
      : action extends { reduce: infer ruleIdx }
      ? ruleIdx extends keyof Productions
        ? Productions[ruleIdx] extends Production
          ? U.PopNStack<
              stack,
              Productions[ruleIdx]["tokens"]["length"]
            > extends infer newStack
            ? newStack extends [infer newTop, ...infer newRestOfStack]
              ? newTop extends number
                ? GetGoto<
                    newTop,
                    Productions[ruleIdx]["name"]
                  > extends infer goto_ta
                  ? goto_ta extends number
                    ? newRestOfStack extends number[]
                      ? actions.EmitProduction<
                          ruleIdx,
                          symbolStack
                        > extends infer newSymbolStack
                        ? newSymbolStack extends Symbol[]
                          ? ParseImpl<
                              tokenInputs,
                              input,
                              [goto_ta, newTop, ...newRestOfStack],
                              i,
                              a,
                              newSymbolStack
                            >
                          : newSymbolStack extends string // it's an error message
                          ? [{ err: newSymbolStack; stack: symbolStack }]
                          : "unreachable: `newSymbolStack` should be either a Symbol[] or a string"
                        : "unreachable: inferring `newSymbolStack` never fails"
                      : "unreachable: `newRestOfStack` should always be a number[]"
                    : goto_ta // else it's an error message so return it
                  : "unreachable: inferring goto_ta never fails"
                : "unreachable: `newTop` should be a number here"
              : "unreachable: `newStack` is always a number"
            : "unreachable: inferring `newStack` never fails"
          : "unreachable: `Productions[ruleIdx]` is not a production"
        : "error: `ruleIdx` is not a key in `Productions`"
      : action extends { accept: true }
      ? [stack, symbolStack] // accept
      : `error: invalid token ${a}`
    : "unreachable: inferring `action` should never fail"
  : "error: stack is empty";

type Parse<tokenInputs extends L.Token[]> =
  TokenIndices<tokenInputs> extends infer input
    ? input extends number[]
      ? ParseImpl<tokenInputs, input, [0], 0, input[0], []>
      : input extends string
      ? input
      : "error: `input` should always be a number[] or string"
    : "unreachable: inferring `input` never fails";

type tokens = L.Lex<"('add' 42 50)">;
type result = Parse<tokens>;
