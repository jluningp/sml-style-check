val x = if NONE = NONE then 0 else 1
fun f y = if y = NONE then 0
          else let val SOME x = y in x end
