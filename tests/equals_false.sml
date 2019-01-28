fun foo y = y = false

val x =
    let
      val y = false
    in
      y = false
    end
