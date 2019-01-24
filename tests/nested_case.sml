val e = fn opt =>
           case opt of
               SOME x => (case x of
                              SOME y => (case y of
                                             SOME _ => true
                                           | NONE => false)
                            | NONE => false)
             | NONE => false
