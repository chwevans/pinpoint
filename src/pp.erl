-module(pp).

-export([
  qand/2,
  qor/2,
  qnot/1,
  eq/2,
  neq/2,
  lt/2,
  gt/2,
  lte/2,
  gte/2
]).

qand(Q1, Q2) -> {'andalso', Q1, Q2}.
qor(Q1, Q2) -> {'orelse', Q1, Q2}.
qnot(Q1) -> {'not', Q1}.

eq(Field, Value) -> op('==', Field, Value).
neq(Field, Value) -> op('/=', Field, Value).

lt(Field, Value) -> op('<', Field, Value).
gt(Field, Value) -> op('>', Field, Value).

lte(Field, Value) -> op('=<', Field, Value).
gte(Field, Value) -> op('>=', Field, Value).


op(Operator, Field, Value) -> {Operator, Field, Value}.
