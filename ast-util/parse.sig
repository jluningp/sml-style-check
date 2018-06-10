signature PARSE =
sig
  val getSource : string -> Source.inputSource

  (* Only use this if you're sure you don't need the sourceMap.
   * Otherwise use SmlFile.parse with getSource *)
  val parseFile : string -> Ast.dec

  val getEnv : Ast.dec -> Environment.staticEnv
end
