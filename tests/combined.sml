fun id x =
    if (if 5 < 10 andalso 10 < 5 then true else false)
    then x
    else x

fun mapPartial f [] = []
  | mapPartial f (x::xs) = if f x = NONE
                           then mapPartial f xs
                           else
                             let
                               val SOME y = id (f x)
                             in
                               [y] @ (mapPartial f xs)
                             end
