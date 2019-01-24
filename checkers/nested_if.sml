structure NestedIf : CHECK =
struct
   open Ast

   fun parts e =
       case e of
           IfExp {elseCase, test, thenCase} => [(elseCase, NONE), (test, NONE), (thenCase, NONE)]
         | _ => []

   fun nested e =
       case AstTraverse.find_exp parts e of
           [] => []
         | L => List.concat (map (fn (exp, _) => AstTraverse.find_exp parts exp) L)

   fun triple_ifs e =
       case e of
           IfExp {elseCase, test, thenCase} =>
           (case List.concat (map nested [elseCase, test, thenCase]) of
                [] => []
              | _ => [(e, NONE)])
         | _ => []

   val check = triple_ifs
   val warning = "more than three nestings of ifs"
   val hint = "try a case expression"
end
