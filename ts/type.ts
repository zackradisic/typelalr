import { Characters, CharTable } from "./chars";
import { States, Tokens } from "./lex_state.gen";
import * as U from "./util";

export type CharRange = {
  from: number;
  to: number;
};

export type SingleCharRange<T extends keyof CharTable> = {
  from: CharTable[T];
  to: U.Add<CharTable[T], 1>;
};

export type State = {
  to: number[];
  labels: CharRange[];
  accepting?: boolean;
};

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
    ? NextState<label, state, U.Add<i, 1>>
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
  fullInput extends string[],
  stateIdx extends number,
  // tokens extends Token[],
  tokens extends string[],
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
            fullInput,
            maybeNewStateIdx,
            tokens,
            lexemeBegin,
            U.Add<forward, 1>,
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
              fullInput,
              0,
              [Tokens[lastAcceptingState[1]], ...tokens],
              U.Add<lastAcceptingState[0], 1>,
              U.Add<lastAcceptingState[0], 1>,
              charBuf,
              undefined
            >
          : "unreachable: accepting state idx should always be a keyof Tokens"
        : [U.IsWhitespace<char>, U.Eq<forward, lexemeBegin>] extends [
            true,
            true
          ]
        ? LexImpl<
            fullInput,
            stateIdx,
            tokens,
            U.Add<lexemeBegin, 1>,
            U.Add<forward, 1>,
            charBuf,
            lastAcceptingState
          >
        : // otherwise it's an invalid symbol, break
          [
            tokens,
            lastAcceptingState,
            `invalid symbol: \`${char}\` at index ${forward}`
          ]
      : "unreachable: inferring `maybeNewStateIdx`"
    : `invalid char: ${char}`
  : [tokens, lastAcceptingState, undefined];

type Lex<fullInput extends string> = LexImpl<
  StringToCharList<fullInput, []>,
  0,
  [],
  0,
  0,
  [],
  undefined
> extends [infer tokens, infer lastAcceptingState, infer error]
  ? error extends string
    ? error
    : lastAcceptingState extends [forward: number, stateIdx: number]
    ? tokens extends any[]
      ? lastAcceptingState[1] extends keyof Tokens
        ? U.Reverse<[Tokens[lastAcceptingState[1]], ...tokens]>
        : lastAcceptingState[1]
      : "unreachable: tokens should be an array"
    : tokens extends any[]
    ? U.Reverse<tokens>
    : "unreachable: tokens should be an array"
  : "unreachable: inferring `tokens` and `lastAcceptingState` should always work";

type toks = Lex<"let x = 2490283409234;">;
