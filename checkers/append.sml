structure Append : CHECK =
struct
  open Ast

  fun find_append e =
      case e of
          MarkExp (e, r) => find_append e
        | VarExp [sym] => Symbol.name sym = "@"
        | FlatAppExp [{fixity, item, region}] => find_append item
        | _ => false

  fun find_singleton e =
      case e of
          MarkExp (e, r) => find_singleton e
        | FlatAppExp [{fixity, item, region}] => find_singleton item
        | ListExp [s] => true
        | _ => false

  fun find_append_singleton e =
      case e of
          FlatAppExp [f1, f2, f3] => if find_singleton (#item f1) andalso find_append (#item f2)
                                     then [(e, NONE)]
                                     else []
        | _ => []

  val check = find_append_singleton
  val warning = "appended singleton to the front of list"
  val hint = "use :: instead"
end
