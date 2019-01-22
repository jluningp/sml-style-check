structure IfBool : CHECK =
struct
   open Ast

   fun find_var e name =
       case e of
           MarkExp (e, r) => find_var e name
         | FlatAppExp [{fixity, item, region}] => find_var item name
         | VarExp [sym] => Symbol.name sym = name
         | _ => false

   fun find_if e =
       case e of
           IfExp {elseCase, test, thenCase} => if find_var thenCase "true"
                                                  andalso find_var elseCase "false"
                                               then [(e, NONE)]
                                               else []
         | _ => []

   val check = find_if
   val warning = "if <bool> then true else false"
   val hint = "you don't need the if!"
end
