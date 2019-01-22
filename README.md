# SML Style Checker
An extendable style checker for Standard ML. This checker works specifically with Standard ML of New Jersey (SML/NJ), 
since it makes use of its Visible Compiler feature for parsing.

## Usage

To call the style checker, compile it with 

```
sml -m sources.cm
```

and call

```
Style.checkStyle <filename>
```

This will output a list of style warnings (in the same format as SML/NJ compiler error/warning messages) that pertain to 
the file. 


## Example
The code being checked in this example can be found in `tests/combined.sml`.

```
$ sml -m sources.cm
Standard ML of New Jersey v110.82 [built: Wed Dec 13 23:38:01 2017]
[scanning sources.cm]
[New bindings added.]
- Style.check_style "tests/combined.sml";
tests/combined.sml:13.32-55 Style: appended singleton to the front of list
   expression: [y] @ (mapPartial f xs)
   hint: use :: instead
tests/combined.sml:7.28-14.33 Style: checked for NONE using if
   expression: if f x = NONE
               then mapPartial f xs
               else let
                  val SOME y = id (f x)
                in
                  ([y] @ (mapPartial f xs))
                end
   hint: use case instead
tests/combined.sml:2.9-54 Style: if <bool> then true else false
   expression: if 5 < 10 andalso 10 < 5
               then true
               else false
   hint: you don't need the if!
val it = () : unit
- 
```

## Extending the Checker
Want to add new style rules?

The style checker's rules can be found in the `checkers/` directory. They are desribed in that directory's [readme](https://github.com/jluningp/sml-style-check/blob/master/checkers/README.md). 

Each rule has three components:
1. A check function. This function should return true when presented with an SML/NJ expression AST (the definition of 
which can be found [here](https://www.smlnj.org/doc/Compiler/pages/ast.html)) that violates the style rule in question, 
and false otherwise. Make sure the check function accounts for Marked, Seq, and FlatAppExp nodes that may be present
in surprising places.

2. A warning string. This is what will be displayed when the rule is violated.

3. A hint string. This will be displayed below the warning string and the expression that violates the guidelines, and 
should give the programmer advice on how to fix the style error. 

For consistency, each rule should be a structure ascribing to the CHECK signature (found in checkers/check.sig).

Once a new rule structure is added to the `checkers/` directory, add it to the `sources.cm` under `(* Add extra rules here *)`.
Then, add a new tuple containing (check, warning, and hint) to the `checkers` list in `main/style.sml`. 

You should also add tests for the new rule to `tests/` and document the rule in the `checkers/` readme. 
