-module(pinpoint_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [can_reference_field_in_query, exact_match_works, nested_queries_work].

indexed_fields() -> [a, b, d].

can_reference_field_in_query(_) ->
  start(),
  [#{a := 2, b := baz, c := data, d := 1}] = pinpoint:search(pinpoint_SUITE, pp:gt(a, d)).

exact_match_works(_) ->
  start(),
  [#{a := 1, b := foo, c := data, d := 1}] = pinpoint:search(pinpoint_SUITE, pp:eq(b, foo)),
  [#{a := 1, b := foo, c := data, d := 1}] = pinpoint:search(pinpoint_SUITE, #{b => foo}).

nested_queries_work(_) ->
  start(),
  [#{a := 2, b := baz, c := data, d := 1}] = pinpoint:search(pinpoint_SUITE, pp:qand(pp:eq(b, baz), pp:gt(a, 0))).

start() ->
  pinpoint:start_link(pinpoint_SUITE),
  pinpoint:insert(pinpoint_SUITE, #{a => 1, b => foo, c => data, d => 1}),
  pinpoint:insert(pinpoint_SUITE, #{a => 0, b => baz, c => data}),
  pinpoint:insert(pinpoint_SUITE, #{a => 2, b => baz, c => data, d => 1}).
