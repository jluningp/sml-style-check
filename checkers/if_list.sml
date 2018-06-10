structure IfList : CHECK =
struct
   open Ast

   fun is_sym sym item =
       case item of
           MarkExp (exp, _) => is_sym sym exp
         | SeqExp [exp] => is_sym sym exp
         | VarExp [s] => Symbol.name s = sym
         | _ => false

   fun is_empty_list item =
       case item of
           MarkExp (exp, _) => is_empty_list exp
         | SeqExp [exp] => is_empty_list exp
         | ListExp [] => true
         | _ => false

   fun find_pair f [] = false
     | find_pair f [x] = false
     | find_pair f (x::y::xs) =
       if f (x, y) orelse f (y, x)
       then true
       else find_pair f (y::xs)

   fun find_option_check exp =
       case exp of
           FlatAppExp (L as _::_::_) => if find_pair (fn (x, y) => is_sym "=" (#item x)
                                                                andalso
                                                                is_empty_list (#item y)) L
                                        then [(exp, NONE)]
                                        else []

         | _ => []

   fun check exp =
       case exp of
           IfExp {thenCase, elseCase, test} => (let
                                                 val found = AstTraverse.find_exp find_option_check test
                                                 val immediate = find_option_check test
                                               in
                                                 case (found, immediate) of
                                                   ([], []) => []
                                                  | _ => [(exp, NONE)]
                                               end)
         | _ => []

   val warning = "checked for [] using if"

   val hint = "use case instead"

end
