local
  open Ast
in

fun fst (x, _) = x

fun regionify region (elem, NONE) = (elem, SOME region)
  | regionify _ (elem, SOME region) = (elem, SOME region)

exception NotFound

fun insert_str str (row, col) lines =
    let
      val (l, r::rs) = List.splitAt (lines, row - 1)
      val e_r = String.explode r
      val (l_r, r_r) = List.splitAt (e_r, col) handle Subscript => (e_r, [])
      val e = String.implode l_r ^ str ^ String.implode r_r
    in
      l @ e::rs
    end

fun markPos _ lines [] = lines
  | markPos sourceMap lines ((left, right)::r) =
    let
      open SourceMap
      val [(left_r, right_r)] = fileregion sourceMap (left, right)
      val (column_left, line_left) = (#column left_r, #line left_r)
      val (column_right, line_right) = (#column right_r, #line right_r)
      val marked = insert_str "%" (line_left, column_left)
                              (insert_str "&" (line_right, column_right) lines)
    in
      markPos sourceMap marked (map (fn (x, y) => (x - 1, y - 1)) r)
    end

fun markFile (source : Source.inputSource) filename regions =
    let
      open TextIO
      val instream = openIn filename
      val outstream = stdOut
      val sourceMap = #sourceMap source
      val lines = List.unfoldl (fn ins => Option.map (fn s => (s, ins)) (inputLine ins)) instream
    in
      concat(map (fn c => case c of
                              #"&" => "</span>"
                            | #"%" => "<span>"
                            | _ => String.str c) (explode(concat(markPos sourceMap lines regions))))
    end

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

fun find_exp f e =
    case e of
        AndalsoExp (e1, e2) => f e1
                               @ f e2
                               @ find_exp f e1
                               @ find_exp f e2
     |  AppExp {argument:exp, function:exp} => f argument
                                               @ f function
                                               @ find_exp f argument
                                               @ find_exp f function
     |  CaseExp {expr:exp, rules:rule list} => f expr
                                               @ find_exp f expr
                                               @ List.concat (map (find_rule f) rules)
     |  CharExp str => []
     |  ConstraintExp {constraint:ty, expr:exp} => f expr
                                                   @ find_exp f expr
     |  FlatAppExp exp_fixitems => List.concatMap (fixitem f) exp_fixitems
     |  FnExp rules => []
     |  HandleExp {expr:exp, rules:rule list} => f expr
                                                 @ find_exp f expr
                                                 @ List.concat (map (find_rule f) rules)
     |  IfExp {elseCase:exp, test:exp, thenCase:exp} => f elseCase
                                                        @ f test
                                                        @ f thenCase
                                                        @ find_exp f elseCase
                                                        @ find_exp f test
                                                        @ find_exp f thenCase
     |  IntExp i => []
     |  LetExp {dec:dec, expr:exp} => f expr
                                      @ find_dec f dec
                                      @ find_exp f expr
     |  ListExp exps => List.concat (map f exps) @ List.concat (map (find_exp f) exps)
     |  MarkExp (e', region) => map (regionify region) (f e' @ find_exp f e')
     |  OrelseExp (e1, e2) => f e1
                              @ f e2
                              @ find_exp f e1
                              @ find_exp f e2
     |  RaiseExp e' => f e'
                       @ find_exp f e'
     |  RealExp str => []
     |  RecordExp fields => List.concat (map (fn (_, e') => f e' @ find_exp f e') fields)
     |  SelectorExp symbol => []
     |  SeqExp exps => List.concat (map (find_exp f) exps) @ List.concat (map f exps)
     |  StringExp strs => []
     |  TupleExp exps => List.concat (map (find_exp f) exps) @ List.concat (map f exps)
     |  VarExp path => []
     |  VectorExp exps => List.concat (map (find_exp f) exps) @ List.concat (map f exps)
     |  WhileExp {expr:exp, test:exp} => f expr
                                         @ f test
                                         @ find_exp f expr
                                         @ find_exp f test
     |  WordExp i => []


and  fixitem f {fixity, item, region} = map (regionify region) (f item @ find_exp f item)

and find_rule f (Rule {exp, pat}) = find_exp f exp

and find_dec f d =
    case d of
      AbsDec strbs => List.concatMap (find_strb f) strbs
     | AbstypeDec {abstycs:db list, body:dec, withtycs:tb list} => []
     | DataReplDec (sym, path) => []
     | DatatypeDec {datatycs:db list, withtycs:tb list} => []
     | DoDec exp => find_exp f exp
     | ExceptionDec ebs => []
     | FctDec fctb => List.concatMap (find_fctb f) fctb
     | FixDec {fixity:fixity, ops:symbol list} => []
     | FsigDec fsigs => []
     | FunDec (fbs, tys) => List.concatMap (find_fbs f) fbs
     | LocalDec (d1, d2) => find_dec f d1 @ find_dec f d2
     | MarkDec (d', region) => map (regionify region) (find_dec f d')
     | OpenDec paths => []
     | OvldDec (sym, ty, exps) => List.concatMap (find_exp f) exps
                                  @ List.concatMap f exps
     | SeqDec decs => List.concatMap (find_dec f) decs
     | SigDec sigs => []
     | StrDec strbs => List.concatMap (find_strb f) strbs
     | TypeDec tb => []
     | ValDec (vbs, tys) => List.concatMap (find_vb f) vbs
     | ValrecDec (rvbs, tys) => List.concatMap (find_rvb f) rvbs


and find_strb f strb =
    case strb of
        MarkStrb (strb, region) => map (regionify region) (find_strb f strb)
      | Strb {constraint, def, name} => find_strexp f def

and find_strexp f strexp =
    case strexp of
        AppStr (longid, strexps) => List.concatMap (fn (s, b) => find_strexp f s) strexps
      | AppStrI (longid, strexps) => List.concatMap (fn (s, b) => find_strexp f s) strexps
      | BaseStr dec => find_dec f dec
      | ConstrainedStr (strexp, sigconst) => find_strexp f strexp
      | LetStr (dec, strexp) => find_dec f dec @ find_strexp f strexp
      | MarkStr (strexp, region) => map (regionify region) (find_strexp f strexp)
      | VarStr path => []

and find_fctb f fctb =
    case fctb of
        MarkFctb (fct, region) => map (regionify region) (find_fctb f fct)
      | Fctb {def, name} => find_fctexp f def

and find_fctexp f fct =
    case fct of
        AppFct (path, strexp, sigexp) => List.concatMap (fn (s, b) => find_strexp f s) strexp
     |  BaseFct {body, constraint, params} => find_strexp f body
     |  LetFct (dec, fctexp) => find_dec f dec @ find_fctexp f fctexp
     |  MarkFct (fctexp, region) => map (regionify region) (find_fctexp f fctexp)
     |  VarFct (path, fsig) => []

and find_fbs f fb =
    case fb of
        MarkFb (fb, region) => map (regionify region) (find_fbs f fb)
      | Fb (clss, b) => List.concatMap (find_clause f) clss

and find_clause f (Clause {exp, pats, resultty}) = f exp @ find_exp f exp

and find_vb f vb =
    case vb of
        MarkVb (vb', region) => map (regionify region) (find_vb f vb')
      | Vb {exp, lazyp, pat} => f exp @ find_exp f exp

and find_rvb f rvb =
    case rvb of
        MarkRvb (rvb', region) => map (regionify region) (find_rvb f rvb')
      | Rvb {exp, fixity, lazyp, resultty, var} => f exp @ find_exp f exp

(*
and pat p =
    case p of
        AppPat {argument, constr} => ()
     |  CharPat str => ()
     |  ConstraintPat {constraint, pattern} => ()
     |  FlatAppPat pat_fixitems => ()
     |  IntPat i => ()
     |  LayeredPat {expPat, varPat} => ()
     |  ListPat pats => ()
     |  MarkPat (p', region) => ()
     |  OrPat pats => ()
     |  RecordPat {def, flexibility} => ()
     |  StringPat str => ()
     |  TuplePat pats => ()
     |  VarPat longid => ()
     |  VectorPat pats => ()
     |  WildPat => ()
     |  WordPat i => ()
*)

fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

fun markAppends filename =
    let
      val source = getSource filename
      val ast = SmlFile.parse source
      val appends = find_dec find_append_singleton ast
      val regions = List.mapPartial (fn (x, y) => y) appends
    in
      writeFile "tmp/output.html" (
        "<style> span { color: red; } </style>"
        ^ "<pre><code>\n"
        ^ (markFile source filename regions)
        ^ "</code></pre>")
    end

end
