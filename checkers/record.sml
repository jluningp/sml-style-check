(* This is specific to 15-150, since students are confused by flex records *)
structure Record : CHECK =
struct
   open Ast

   fun check e =
       case e of
           RecordExp _ => [(e, NONE)]
         | _ => []

   val warning = "using record"
   val hint = "use a tuple instead"
end
