structure Style : STYLE =
struct
  (* To add more style rules, add structures in checker/ and then add them to the list here *)
  val checkers = [(Append.check, Append.warning, Append.hint),
                  (IfOption.check, IfOption.warning, IfOption.hint),
                  (IfList.check, IfList.warning, IfList.hint)]

  fun sourceloc_to_string (left : SourceMap.sourceloc, right : SourceMap.sourceloc) =
      let
        val left_line = #line left
        val right_line = #line right
        val left_str = Int.toString left_line ^ "." ^ Int.toString (#column left)
        val right_str = if left_line = right_line
                        then Int.toString (#column right)
                        else Int.toString right_line ^ "." ^ Int.toString (#column right)
      in
        left_str ^ "-" ^ right_str
      end

  fun region_to_string sourceMap (left, right) : string =
      let
        val regions = SourceMap.fileregion sourceMap (left, right)
      in
        concat (map sourceloc_to_string regions)
      end

  fun warnings (source : Source.inputSource, ast, env) (check, warning, hint) =
      let
        val filename = #fileOpened source
        val sourceMap = #sourceMap source
        val found = AstTraverse.find_dec check ast
        val indent = "               "
        fun printout (exp, region_opt) = (let
                                           val lines = case region_opt of
                                                           SOME region => region_to_string sourceMap region
                                                         | NONE => ""
                                         in
                                           filename ^ ":" ^ lines ^
                                           " Style: " ^ warning ^ "\n"
                                           ^ "   expression: " ^ Lint.pp_exp exp env indent ^ "\n"
                                           ^ "   hint: " ^ hint ^ "\n"
                                         end)
      in
        concat (map printout found)
      end

  fun check_style filename =
      let
        val source = Parse.getSource filename
        val ast = SmlFile.parse source
        val env = Parse.getEnv ast
      in
        print (concat (map (warnings (source, ast, env)) checkers))
      end
end
