import { Characters, CharTable } from "./chars";
import { States, Tokens } from "./lex_state.gen";
import * as U from "./util";

export type CharRange = {
  from: number;
  to: number;
};

export type SingleCharRange<T extends keyof CharTable> = {
  from: CharTable[T];
  to: U.Succ<CharTable[T]>;
};

export type State = {
  to: number[];
  labels: CharRange[];
  accepting?: boolean;
};

export type TokenDef = {
  kind: string;
  hasValue: boolean;
};

export type Token = {
  kind: string;
  value?: string;
};

type TokenDefToToken<
  tokenDefKey extends number,
  input extends string,
  lexemeBegin extends number,
  forward extends number
> = tokenDefKey extends keyof Tokens
  ? Tokens[tokenDefKey]["hasValue"] extends true
    ? {
        kind: Tokens[tokenDefKey]["kind"];
        value: U.Substring<input, lexemeBegin, forward>;
      }
    : { kind: Tokens[tokenDefKey]["kind"] }
  : never;

type RangeContains<range extends CharRange, labelIdx extends number> = [
  U.Gte<labelIdx, range["from"]>,
  U.Lt<labelIdx, range["to"]>
] extends [true, true]
  ? true
  : false;

type NextState<
  label extends number,
  state extends State,
  i extends number
> = state["labels"][i] extends CharRange
  ? RangeContains<state["labels"][i], label> extends true
    ? state["to"][i]
    : i extends number
    ? NextState<label, state, U.Succ<i>>
    : "unreachable: `i` should always be a number"
  : undefined;

type StateAccepts<state extends State> = state["accepting"] extends true
  ? true
  : false;

type StringToCharList<
  input extends string,
  ret extends string[]
> = input extends ""
  ? U.Reverse<ret>
  : input extends `${infer char}${infer rest}`
  ? char extends Characters[keyof Characters]
    ? StringToCharList<rest, [char, ...ret]>
    : `invalid char: ${char}`
  : U.Reverse<ret>;

type LexImpl<
  inputStr extends string,
  fullInput extends string[],
  stateIdx extends number,
  tokens extends Token[],
  lexemeBegin extends number,
  forward extends number,
  charBuf extends string[],
  lastAcceptingState extends [forward: number, stateIdx: number] | undefined
> = fullInput[forward] extends `${infer char}`
  ? char extends Characters[keyof Characters]
    ? NextState<
        CharTable[char],
        States[stateIdx],
        0
      > extends infer maybeNewStateIdx
      ? maybeNewStateIdx extends number
        ? // We have a matching edge
          LexImpl<
            inputStr,
            fullInput,
            maybeNewStateIdx,
            tokens,
            lexemeBegin,
            U.Succ<forward>,
            charBuf,
            StateAccepts<States[maybeNewStateIdx]> extends true
              ? [forward, maybeNewStateIdx]
              : lastAcceptingState
          >
        : // No matching edge
        lastAcceptingState extends [
            lastAcceptingIdx: number,
            lastAcceptingStateIdx: number
          ]
        ? lastAcceptingState[1] extends keyof Tokens
          ? LexImpl<
              inputStr,
              fullInput,
              0,
              [
                TokenDefToToken<
                  lastAcceptingState[1],
                  inputStr,
                  lexemeBegin,
                  forward
                >,
                ...tokens
              ],
              U.Succ<lastAcceptingState[0]>,
              U.Succ<lastAcceptingState[0]>,
              charBuf,
              undefined
            >
          : "unreachable: accepting state idx should always be a keyof Tokens"
        : [U.IsWhitespace<char>, U.Eq<forward, lexemeBegin>] extends [
            true,
            true
          ]
        ? LexImpl<
            inputStr,
            fullInput,
            stateIdx,
            tokens,
            U.Succ<lexemeBegin>,
            U.Succ<forward>,
            charBuf,
            lastAcceptingState
          >
        : // otherwise it's an invalid symbol, break
          [
            tokens,
            lastAcceptingState,
            lexemeBegin,
            `invalid symbol: \`${char}\` at index ${forward}`
          ]
      : "unreachable: inferring `maybeNewStateIdx` should always work"
    : `invalid char: ${char}`
  : [tokens, lastAcceptingState, lexemeBegin, undefined];

type Lex<fullInput extends string> = LexImpl<
  fullInput,
  StringToCharList<fullInput, []>,
  0,
  [],
  0,
  0,
  [],
  undefined
> extends [
  infer tokens,
  infer lastAcceptingState,
  infer lexemeBegin,
  infer error
]
  ? error extends string
    ? error
    : lastAcceptingState extends [forward: number, stateIdx: number]
    ? tokens extends any[]
      ? lastAcceptingState[1] extends keyof Tokens
        ? lexemeBegin extends number
          ? U.Reverse<
              [
                TokenDefToToken<
                  lastAcceptingState[1],
                  fullInput,
                  lexemeBegin,
                  lastAcceptingState[0]
                >,
                ...tokens
              ]
            >
          : "unreachable: `lexemeBegin` begin should always be a number"
        : lastAcceptingState[1]
      : "unreachable: tokens should be an array"
    : tokens extends any[]
    ? U.Reverse<tokens>
    : "unreachable: tokens should be an array"
  : "unreachable: inferring `tokens` and `lastAcceptingState` should always work";

type toks = Lex<`
 let foo = 420;
 let foo = 12381927391827389123;
`>;
