pinpoint
========

An easily controlled ETS table that holds onto any map of data.
The behaviour implementor can decide which fields are indexed, making them available to the query language.

The query language is designed to be simpler than writing match specs as you only need to know field names.

Examples
--------
* Query via exact match:
```erlang
pinpoint:query(implementor, #{field1 => a, field2 => 2}).
```

* Query via query builder:
```erlang
pinpoint:query(implementor, pp:qand(pp:eq(field1, a), pp:eq(field2, 2)))
```
