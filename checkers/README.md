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
