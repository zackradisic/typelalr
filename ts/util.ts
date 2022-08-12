import { SuccTable, PredTable } from "./arith_table.gen";

export type Err<T extends string> = `error: ${T}`;

export type Negate<A extends boolean> = A extends true ? false : true;

export type Reverse<T extends any[]> = ReverseImpl<T, []>;

type ReverseImpl<T extends any[], State extends any[] = []> = T extends [
  infer A,
  ...infer B
]
  ? ReverseImpl<B, [A, ...State]>
  : State;

export type Tail<T extends any[]> = T extends [...infer first, ...infer rest]
  ? rest
  : [];

export type IsWhitespace<T extends string> = T extends " " | "\n"
  ? true
  : false;

export type Substring<
  str extends string,
  start extends number,
  end extends number
> = SubstringImpl<str, start, end, 0>;

type SubstringImpl<
  str extends string,
  start extends number,
  end extends number,
  iStart extends number
> = iStart extends start
  ? CutString<str, "", end, 0>
  : str extends `${infer _char}${infer rest}`
  ? SubstringImpl<rest, start, Pred<end>, Succ<iStart>>
  : undefined;

type CutString<
  str extends string,
  ret extends string,
  len extends number,
  i extends number
> = i extends len
  ? ret
  : str extends `${infer char}${infer rest}`
  ? CutString<rest, `${ret}${char}`, len, Succ<i>>
  : ret;

export type Eq<A extends number, B extends number> = A extends B ? true : false;
export type Succ<T> = T extends keyof SuccTable ? SuccTable[T] : never;
export type Pred<T> = T extends 0
  ? 0
  : T extends keyof PredTable
  ? PredTable[T]
  : never;

export type Gte<A extends B, B extends number> = A extends B
  ? true
  : GtImpl<A, B>;
export type Gt<A extends B, B extends number> = A extends B
  ? false
  : GtImpl<A, B>;

export type Lte<A extends B, B extends number> = A extends B
  ? true
  : Negate<GtImpl<A, B>>;
export type Lt<A extends B, B extends number> = A extends B
  ? false
  : Negate<GtImpl<A, B>>;

// A > B
export type GtImpl<A extends number, B extends number> = B extends 0
  ? true
  : A extends 0
  ? false
  : GtImpl<Pred<A>, Pred<B>>;

export type Add<A extends number, B extends number> = B extends 0
  ? A
  : Add<Succ<A>, Pred<B>>;
// Only works if B <= A
export type Sub<A extends number, B extends number> = B extends 0
  ? A
  : Sub<Pred<A>, Pred<B>>;
