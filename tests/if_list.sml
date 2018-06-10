fun length L = if L = [] then 0
               else
                 let
                   val x::xs = L
                 in
                   1 + length xs
                 end

fun foo f x = if f x = [] then 0 else 1
