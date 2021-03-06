val y = [5] @ []
val z = 5::[]

fun map f [] = []
  | map f (x::xs) = [f x] @ (map f xs)

fun also_map f e =
    case e of
        [] => []
      | x::xs => (let
                   val call = f x
                   val res = [call] @ (map f xs)
                 in
                   res
                 end)
