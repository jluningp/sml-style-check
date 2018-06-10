fun mapPartial f [] = []
  | mapPartial f (x::xs) = if f x = NONE
                           then mapPartial f xs
                           else
                             let
                               val SOME y = f x
                             in
                               [y] @ (mapPartial f xs)
                             end
