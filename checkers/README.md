## List of Style Rules

Extend this when you add new rules.

#### Rule: Use x::L, not [x]@L

When adding an element `x` to the front of a list `L`, it is bad style to use `[x] @ L` rather than `x::L`.

Files: `checkers/append.sml`, `tests/append.sml`

#### Rule: Use case, not if, for options

When checking if a value `v` is `NONE`, it is bad style to use `if v = NONE then ...` rather than `case v of NONE => ...`

Files: `checkers/if_option.sml`, `tests/if_option.sml`

#### Rule: Use case, not if, for lists

When checking if a value `v` is `nil`, it is bad style to use `if v = [] then ...` rather than `case v of [] => ...`

Files: `checkers/if_list.sml`, `tests/if_list.sml`

#### Rule: Remove unnecessary ifs 

Never write `if exp then true else false`. Use `exp` instead.

Files: `checkers/if_bool.sml`, `tests/if_bool.sml`

#### Rule: Don't nest ifs more than three levels deep

Opt to use case expressions instead.

Files: `checkers/nested_if.sml`, `tests/nested_if.sml`

#### Rule: Don't nest cases more than three levels deep

Case on tuples or use helper functions.

Files: `checkers/nested_case.sml`, `tests/nested_case.sml`
