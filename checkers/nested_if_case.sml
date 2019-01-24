structure NestedIfCase : CHECK =
struct
   open Ast

   fun rule (Rule {exp, pat=_}) = exp

   fun parts e =
       case e of
           CaseExp {expr, rules} => map (fn e => (e, NONE)) (expr::(map rule rules))
         | IfExp {test, thenCase, elseCase} => [(test, NONE), (thenCase, NONE), (elseCase, NONE)]
         | _ => []

   fun nested (e, _) =
       case AstTraverse.find_exp parts e of
           [] => []
         | L => List.concat (map (fn (exp, _) => AstTraverse.find_exp parts exp) L)

   fun nested3 (e, _) =
       case AstTraverse.find_exp parts e of
           [] => []
         | L => List.concat (map nested L)

   fun triple_cases e =
       case List.concat (map nested3 (parts e)) of
           [] => []
         | _ => [(e, NONE)]

   val check = triple_cases
   val warning = "more than three nestings of cases/ifs"
   val hint = "try using nested patterns or writing a helper function"
end
