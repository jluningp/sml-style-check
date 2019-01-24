structure IfBoolInv : CHECK =
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
           IfExp {elseCase, test, thenCase} => if find_var thenCase "false"
                                                  andalso find_var elseCase
                                                  "true"
                                               then [(e, NONE)]
                                               else []
         | _ => []

   val check = find_if
   val warning = "if <bool> then false else true"
   val hint = "you don't need the if just use a not!"
end
