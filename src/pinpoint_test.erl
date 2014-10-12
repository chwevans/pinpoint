-module(pinpoint_test).

-export([indexed_fields/0, test_data/0]).

indexed_fields() -> [a, b, c].

test_data() ->
  pinpoint:start_link(pinpoint_test),
  pinpoint:insert(pinpoint_test, #{a => 1, b => foo, d => data}),
  pinpoint:insert(pinpoint_test, #{a => 0, b => baz, d => data}),
  pinpoint:insert(pinpoint_test, #{a => 2, b => baz, d => data}).
