signature AST_TRAVERSE =
sig
  (* An Ast.dec is returned by the parser, so it's easy to pass the parser output straight into
   * find_dec. However, other functions can be exposed as needed. *)
  val find_dec : (Ast.exp -> (Ast.exp * (Ast.region option)) list) -> Ast.dec -> (Ast.exp * (Ast.region option)) list
end
