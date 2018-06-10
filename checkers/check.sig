signature CHECK =
sig
  val check : Ast.exp -> ((Ast.exp * Ast.region option) list)
  val warning : string
  val hint : string
end
