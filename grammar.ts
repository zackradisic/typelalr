import { Regex, ZeroOrMore, OneOrMore } from "./grammar_util";

type E = [E, "+", T] | T

type T = [T, "*", F]

type F = ["(", E, ")"] | [num]

type num = Regex<"[1-9]+[0-9]*">


// export const E = [
//   (e: typeof E, plus: "+", t: typeof T) => ({ kind: "plus", left: e, right: e}),
// ]


