structure Parse : PARSE =
struct
  fun getSource filename =
      let
        val stream = TextIO.openIn filename
        val interactive = false
        val consumer = ErrorMsg.defaultConsumer ()
      in
        Source.newSource (filename, stream, interactive, consumer)
      end

  fun parseFile filename = SmlFile.parse (getSource filename)

  fun getEnv ast =
      let
        val compManagerHook : {
          manageImport : Ast.dec * EnvRef.envref -> unit,
          managePrint :  Symbol.symbol * EnvRef.envref -> unit,
          getPending : unit -> Symbol.symbol list
        } ref = ref {
              manageImport = fn _ => (),
              managePrint = fn _ => (),
              getPending = fn () => []
            }

        val loc = EnvRef.loc ()
        val base = EnvRef.base ()
        val _ = #manageImport (!compManagerHook) (ast, loc)

        fun getenv () = Environment.layerEnv (#get loc (), #get base ())
        val env = #static (getenv ())
      in
        env
      end
end
