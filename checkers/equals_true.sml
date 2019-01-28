structure EqualsTrue : CHECK =
struct
   open Ast

   fun is_sym sym item =
       case item of
           MarkExp (exp, _) => is_sym sym exp
         | SeqExp [exp] => is_sym sym exp
         | VarExp [s] => Symbol.name s = sym
         | _ => false

   fun find_pair f [] = false
     | find_pair f [x] = false
     | find_pair f (x::y::xs) =
       if f (x, y) orelse f (y, x)
       then true
       else find_pair f (y::xs)

   fun check exp =
       case exp of
           FlatAppExp (L as _::_::_) => if find_pair (fn (x, y) => is_sym "=" (#item x)
                                                                andalso
                                                                is_sym "true" (#item y)) L
                                        then [(exp, NONE)]
                                        else []

         | _ => []

   val warning = "<exp> = true"

   val hint = "don't need the = true"

end
