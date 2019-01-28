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

#### Rule: Use not, instead of if for negation

When inverting a boolean value `v`, it is bad style to use `if v then false else true` rather than `not v`

Files: `checkers/if_bool_inv.sml`, `tests/if_bool_inv.sml`

#### Rule: Use let, not case for pattern matching

Instead of using a one armed case such as `case z of (x, _) => x + 1`, it is better style to use `let val (x, _) = z in x + 1 end`.

Files: `checkers/case_one_arm.sml`, `tests/case_one_arm.sml`

#### Rule: Remove unnecessary ifs 

Never write `if exp then true else false`. Use `exp` instead.

Files: `checkers/if_bool.sml`, `tests/if_bool.sml`

#### Rule: Don't nest ifs and cases more than three levels deep

Use nested patterns, case on tuples, or create helper functions instead.

Files: `checkers/nested_if_case.sml`, `tests/nested_if_case.sml`
