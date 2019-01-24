val foo = fn b => if b
                  then if b
                       then if b
                            then 0
                            else 1
                       else 1
                  else 1

val bar = if foo < 1
          then 0
          else if foo > 1
          then 0
          else if foo > 0
          then 0
          else 1

val baz = if bar < 1
          then if bar > 0
               then 1
               else if bar = 0
               then 1
               else 0
          else 0
