structure IfOption : CHECK =
struct
   open Ast

   fun is_sym sym item =
       case item of
           MarkExp (exp, _) => is_sym sym exp
         | SeqExp [exp] => is_sym sym exp
         | VarExp [s] => Symbol.name s = sym
         | _ => false

   fun find_option_check exp =
       case exp of
           FlatAppExp [f1, f2, f3] => is_sym "=" (#item f2) andalso is_sym "NONE" (#item f3)
         | _ => false

   fun check exp =
       case exp of
           IfExp {thenCase, elseCase, test} => if find_option_check test
                                               then [(exp, NONE)]
                                               else []
         | _ => []

   val warning = "checked for NONE using if"

   val hint = "use case instead"

end
