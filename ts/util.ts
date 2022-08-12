import { SuccTable, PredTable } from "./arith_table.gen";

export type Err<T extends string> = `error: ${T}`;

export type Eq<A extends number, B extends number> = A extends B ? true : false;
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

export type IsWhitespace<T extends string> = T extends " " | " \n"
  ? true
  : false;

// export type Substring<str extends string, start extends number, end extends number>

export type Add<T, K> = T extends keyof SuccTable ? SuccTable[T] : never;
export type Sub<T> = T extends 0
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
  : GtImpl<Sub<A>, Sub<B>>;
