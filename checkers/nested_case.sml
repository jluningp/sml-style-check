structure NestedCase : CHECK =
struct
   open Ast

   fun rule (Rule {exp, pat}) = exp

   fun parts e =
       case e of
           CaseExp {expr, rules} => map (fn e => (e, NONE)) (expr::(map rule rules))
         | _ => []

   fun nested (e, _) =
       case AstTraverse.find_exp parts e of
           [] => []
         | L => List.concat (map (fn (exp, _) => AstTraverse.find_exp parts exp) L)

   fun triple_cases e =
       case e of
           CaseExp {expr, rules} =>
           (case List.concat (map nested (parts e)) of
                [] => []
              | _ => [(e, NONE)])
         | _ => []

   val check = triple_cases
   val warning = "more than three nestings of cases"
   val hint = "try casing on a tuple"
end
