structure CaseOneArm : CHECK =
struct
   open Ast

   fun find_case e =
       case e of
           CaseExp {expr=_, rules=[_]} => [(e, NONE)]
         | _ => []

   val check = find_case
   val warning = "case expression has one arm"
   val hint = "Use a let for pattern matching instead of a one armed case"
end
