structure HashSelector : CHECK =
struct
   open Ast

   fun check e =
       case e of
           SelectorExp _ => [(e, NONE)]
         | _ => []

   val warning = "using # to access record or tuple"
   val hint = "try using pattern matching instead"
end
