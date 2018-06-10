signature LINT =
sig
  type indent = string
  val loadFile : string -> Ast.dec * StaticEnv.staticEnv
  val writeFile : string -> string -> unit
  val pp_pat : Ast.pat -> indent -> bool -> string
  val pp_exp : Ast.exp -> StaticEnv.staticEnv -> indent -> string
  val pp_dec : Ast.dec -> StaticEnv.staticEnv -> indent -> string
end

structure Lint : LINT =
struct
   open Ast Fixity VarCon Types Tuples

   type indent = string

   (* PARSING/ENVIRONMENT *)
   fun loadFile filename =
       let
         val stream = TextIO.openIn filename
         val interactive = false
         val consumer = ErrorMsg.defaultConsumer ()
         val source = Source.newSource (filename, stream, interactive, consumer)
         val ast = SmlFile.parse source

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
         (ast, env)
       end

   fun writeFile filename content =
       let val fd = TextIO.openOut filename
           val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
           val _ = TextIO.closeOut fd
       in () end


   (* FORMATTING *)
   fun intercalate sym [] = ""
     | intercalate sym (x::xs) =
       let
         fun intercalate' sym [] = ""
           | intercalate' sym (y::ys) = sym ^ y ^ (intercalate' sym ys)
       in
         x ^ (intercalate' sym xs)
       end

   fun newline s = "\n" ^ s

   fun pp_seq sep pr seq =
       intercalate sep (map pr seq)

   fun incr_indent indent 0 = indent
     | incr_indent indent n = incr_indent (indent ^ " ") (n - 1)

   fun pp_het_seq f sym pr [] = ""
     | pp_het_seq f sym pr [x] = pr x
     | pp_het_seq f (sym1, sym2) pr (x::xs) =
       let
         fun inner_het [] = ""
           | inner_het [y] = ""
           | inner_het (y::z::ys) =
             (if f y orelse f z
             then sym2
             else sym1)
             ^ pr z ^ (inner_het (z::ys))
       in
         pr x ^ inner_het (x::xs)
       end

   fun lpcond atom = if atom then "(" else ""
   fun rpcond atom = if atom then ")" else ""

   (* TUPLES *)
   fun checkpat (n,nil) = true
     | checkpat (n, (sym,_)::fields) = Symbol.eq(sym, numlabel n) andalso checkpat(n+1,fields)

   fun checkexp (n, nil) = true
     | checkexp (n, (sym,exp)::fields) =
       Symbol.eq(sym, numlabel n) andalso checkexp (n+1, fields)

   fun isTUPLEpat (RecordPat{def=[_],...}) = false
     | isTUPLEpat (RecordPat{def=defs,flexibility=false}) = checkpat(1,defs)
     | isTUPLEpat _ = false

   fun isTUPLEexp (RecordExp[_]) = false
     | isTUPLEexp (RecordExp fields) = checkexp(1,fields)
     | isTUPLEexp (MarkExp(a,_)) = isTUPLEexp a
     | isTUPLEexp _ = false

   (* FIXITY *)
   fun lookFIX (env,sym) =
       Lookup.lookFix (env,Symbol.fixSymbol(Symbol.name sym))

   val nullFix = INfix(0,0)
   val infFix = INfix(1000000,100000)
   fun strongerL(INfix(_,m),INfix(n,_)) = m >= n
     | strongerL  _ = false                 (* should not matter *)
   fun strongerR(INfix(_,m),INfix(n,_)) = n > m
     | strongerR _ = true                  (* should not matter *)

   (* SYMBOLS *)
   fun pp_sym s = Symbol.name s

   fun pp_symbol_list symbols =
       pp_seq "." pp_sym symbols


   (* TYPES *)
   fun ppTyvar tyv =
       case tyv of
           Tyv s => pp_sym s
         | MarkTyv (t, r) => ppTyvar t

   fun pp_typ typ indent =
       let
         fun strength ty =
             case ty of
                 VarTy(_) => 1
              |  ConTy(tycon, args) =>
                 (case tycon
                   of [tyc] =>
                      if Symbol.eq(Symbol.tycSymbol("->"), tyc) then 0
                      else 2
                   |  _ => 2)
              |  RecordTy _ => 2
              |  TupleTy _ => 1
              |  _ => 2

         fun ppTypeArgs tys =
             case tys of
                 [] => ""
               | [ty] =>
                 if strength ty <= 1
                 then "(" ^ pp_typ ty indent ^ ")"
                 else pp_typ ty indent
               | _ =>
                 "("
                 ^ pp_seq "," (fn t => pp_typ t indent) tys
                 ^ ")"
       in
         case typ of
             VarTy t => ppTyvar t
           | ConTy (tycon, []) => pp_symbol_list tycon
           | ConTy (tycon, args) =>
             (case tycon of
                  [tyc] =>
                  if Symbol.eq (Symbol.tycSymbol("->"), tyc)
                  then (case args of
                            [dom, ran] => pp_typ dom indent
                                          ^ " -> "
                                          ^ pp_typ ran indent
                          | _ => raise Fail "-> tycon needs 2 args")
                  else ppTypeArgs args
                       ^ " "
                       ^ pp_sym tyc
                | _ => ppTypeArgs args
                       ^ " "
                       ^ pp_symbol_list tycon)
           | RecordTy s =>
             let
               val records = pp_seq "," (fn (sym, tv) => pp_sym sym ^ " : " ^ pp_typ tv indent) s
               val f_record = if String.size records > 80
                              then pp_seq ("," ^ newline (indent ^ "  "))
                                          (fn (sym, tv) => pp_sym sym ^ " : " ^ pp_typ tv indent) s
                              else records

             in
               "{"
               ^ f_record
               ^ "}"
             end
           | TupleTy t =>
             "("
             ^ pp_seq " * " (fn p => pp_typ p indent) t
             ^ ")"
           | MarkTy (t, r) => pp_typ t indent
       end

   (* PATTERNS *)
   fun pp_pat pat indent atom =
       let
         fun pp_pat' pat ident atom =
             case pat of
                 WildPat => "_"
               | VarPat p => pp_symbol_list p
               | IntPat i => IntInf.toString i
               | WordPat w => "0w" ^ IntInf.toString w
               | StringPat s => "\"" ^ s ^ "\""
               | CharPat c => "#" ^  "\"" ^ c ^ "\""
               | LayeredPat {varPat, expPat} =>
                 lpcond atom
                 ^ pp_pat' varPat indent false ^ " as " ^ pp_pat' expPat indent false
                 ^ rpcond atom
               | RecordPat {def=[], flexibility} =>
                 if flexibility then "{...}"
                 else "()"
               | (r as RecordPat {def, flexibility}) =>
                 if isTUPLEpat r
                 then "("
                      ^ pp_seq "," (fn (sym, pat) => pp_pat' pat indent false) def
                      ^ ")"
                 else "{"
                      ^ pp_seq "," (fn (sym, pat) => pp_sym sym ^ "=" ^ pp_pat' pat indent false) def
                      ^ (if flexibility
                         then ", ..."
                         else "")
                      ^ "}"
               | ListPat [] => "[]"
               | ListPat l => "["
                              ^ pp_seq "," (fn p => pp_pat' p indent false) l
                              ^ "]"
               | TuplePat t => "("
                               ^ pp_seq "," (fn p => pp_pat' p indent false) t
                               ^ ")"
               | FlatAppPat fap =>
                 lpcond atom
                 ^ pp_seq " " (fn {item, fixity, region} => pp_pat' item indent true) fap
                 ^ rpcond atom
               | ConstraintPat {pattern, constraint} =>
                 lpcond atom
                 ^ pp_pat' pattern indent false
                 ^ " : "
                 ^ pp_typ constraint indent
                 ^ rpcond atom
               | AppPat {constr, argument} =>
                 lpcond atom
                 ^ pp_pat' constr indent false ^ " as " ^ pp_pat' argument indent false
                 ^ rpcond atom
               | VectorPat [] => "#[]"
               | VectorPat v => "#["
                                ^ pp_seq "," (fn p => pp_pat' p indent false) v
                                ^ "]"
               | MarkPat (pat, (s, e)) => pp_pat' pat indent atom
               | OrPat orpat => "("
                                ^ pp_seq "| " (fn p => pp_pat' p indent false) orpat
                                ^ ")"
       in
         pp_pat' pat indent atom
       end

   (* EXPRESSIONS *)

   fun pp_exp exp (env : StaticEnv.staticEnv) indent =
       let
         fun pp_exp' exp atom indent =
             case exp of
                 VarExp p => pp_symbol_list p
               | FnExp rules =>
                 lpcond atom
                 ^ "fn " ^ (pp_seq (newline indent ^ " | ") (fn r => pp_rule r env (indent ^ " ")) rules)
                 ^ rpcond atom
               | FlatAppExp fap =>
                 pp_seq " " (fn {item, fixity, region} => pp_exp' item true indent) fap
               | AppExp e => (lpcond atom)
                             ^ pp_app_exp (AppExp e, nullFix, nullFix) atom indent
                             ^ (rpcond atom)
               | CaseExp {expr, rules} =>
                 "(case "
                 ^ pp_exp' expr true indent
                 ^ " of"
                 ^ newline (indent ^ "   ")
                 ^ (pp_seq (newline indent ^ " | ") (fn r => pp_rule r env (indent ^ " ")) rules)
                 ^ ")"
               | LetExp {dec, expr} =>
                 "let"
                 ^ newline (indent ^ "  ")
                 ^ pp_dec dec env (indent ^ "  ")
                 ^ indent
                 ^ "in"
                 ^ newline (indent ^ "  ")
                 ^ pp_exp' expr false (indent ^ "  ")
                 ^ newline indent
                 ^ "end"
               | SeqExp exps =>
                 let
                   fun parenThunk () =
                       "("
                       ^ pp_seq (";" ^ newline indent) (fn exp => pp_exp' exp false indent) exps
                       ^ ")"
                   fun subExpCount (MarkExp (expr, _)) = subExpCount expr
                     | subExpCount (FlatAppExp subexps) = length subexps
                     | subExpCount _ = 1
                 in
                   case exps
                    of [expr] => if (subExpCount expr) < 2
                                 then pp_exp' expr atom indent
                                 else parenThunk()
                     | _ => parenThunk()
                 end
               | IntExp i => IntInf.toString i
               | WordExp w => "0w" ^ IntInf.toString w
               | RealExp r => r
               | StringExp s => "\"" ^ (String.toString s) ^ "\""
               | CharExp c => "#" ^  "\"" ^ c ^ "\""
               | (r as RecordExp fields) =>
                 if isTUPLEexp r
                 then "("
                      ^ pp_seq ", " (fn (_, exp) => pp_exp' exp false indent) fields
                      ^ ")"
                 else "{"
                      ^ pp_seq ", " (fn (name, exp) => (pp_sym name) ^ "=" ^ pp_exp' exp false indent) fields
                      ^ "}"
               | ListExp p => "["
                              ^ pp_seq ", " (fn exp => pp_exp' exp false indent) p
                              ^ "]"
               | TupleExp p => "("
                               ^ pp_seq ", " (fn exp => pp_exp' exp false indent) p
                               ^ ")"
               | SelectorExp name =>
                 lpcond atom
                 ^ "#"
                 ^ pp_sym name
                 ^ rpcond atom
               | ConstraintExp {expr, constraint} =>
                 lpcond atom
                 ^ pp_exp' expr false indent
                 ^ ":"
                 ^ pp_typ constraint indent
                 ^ rpcond atom
               | HandleExp {expr, rules} =>
                 lpcond atom
                 ^ pp_exp' expr atom indent
                 ^ " handle "
                 ^ pp_seq (newline indent ^ " | ") (fn r => pp_rule r env (indent ^ " ")) rules
                 ^ rpcond atom
               | RaiseExp exp =>
                 lpcond atom
                 ^ " raise "
                 ^ pp_exp' exp true indent
                 ^ rpcond atom
               | IfExp {test, thenCase, elseCase} =>
                 lpcond atom
                 ^ "if "
                 ^ pp_exp' test false (indent ^ " ")
                 ^ newline indent
                 ^ "then "
                 ^ pp_exp' thenCase false (indent ^ " ")
                 ^ newline indent
                 ^ "else "
                 ^ pp_exp' elseCase false (indent ^ " ")
                 ^ rpcond atom
               | AndalsoExp (e1, e2) =>
                 lpcond atom
                 ^ pp_exp' e1 true indent
                 ^ " andalso "
                 ^ pp_exp' e2 true indent
                 ^ rpcond atom
               | OrelseExp (e1, e2) =>
                 lpcond atom
                 ^ pp_exp' e1 true indent
                 ^ " orelse "
                 ^ pp_exp' e2 true indent
                 ^ rpcond atom
               | WhileExp {test, expr} =>
                 "while "
                 ^ pp_exp' test false indent
                 ^ newline indent
                 ^ "do "
                 ^ newline (indent ^ " ")
                 ^ pp_exp' expr false (indent ^ " ")
               | VectorExp [] => "#[]"
               | VectorExp exps =>
                 "#["
                 ^ pp_seq "," (fn exp => pp_exp' exp false indent) exps
                 ^ "]"
               | MarkExp (exp, (s, e)) =>
                 (* If we want to modify a file in place, then the
                  * source location information will be useful, so
                  * we'll want to change this *)
                 pp_exp' exp atom indent

         and pp_app_exp (exp, left, right) (atom : bool) indent =
             let
               fun stripMark (MarkExp(a,_)) = stripMark a
                 | stripMark x = x

               fun fixitypp(name, rand, leftFix, rightFix) =
                   let
                     val dname = SymPath.toString (SymPath.SPATH name)
                     val thisFix = case name of
                                       [id] => lookFIX (env, id)
                                     | _ => NONfix
                     fun prNon exp = pp_exp' exp true indent
                   in
                     case thisFix of
                         INfix _ =>  (case stripMark rand of
                                          RecordExp [(_, pl), (_, pr)] =>
                                          let
                                            val atom = strongerL (leftFix, thisFix)
                                                       orelse strongerR (thisFix, rightFix)
                                            val (left, right) = if atom then (nullFix, nullFix)
                                                                else (leftFix, rightFix)
                                          in
                                            lpcond atom
                                            ^ pp_app_exp (pl, left, thisFix) atom indent
                                            ^ " " ^ dname ^ " "
                                            ^ pp_app_exp (pr, thisFix, right) atom indent
                                            ^ rpcond atom
                                          end
                                        | e' => prNon e')
                       | NONfix => prNon rand
                   end

               fun appPrint (AppExp {function=rator, argument=rand}, l, r) =
                   (case stripMark rator of
                        VarExp v => fixitypp (v, rand, l, r)
                     |  rator => (pp_exp' rator atom indent) ^ " " ^ (pp_exp' rand atom indent))
                 | appPrint (MarkExp (exp, (s, e)), l, r) =
                   (* If we want to modify a file in place, then the
                    * source location information will be useful, so
                    * we'll want to change this *)
                   appPrint (exp, l, r)
                 | appPrint _ = raise Fail "Application invalid"
             in
               appPrint (exp, left, right)
             end
       in
         pp_exp' exp false indent
       end

   and pp_rule (Rule {pat, exp}) env indent =
       let
         val patrn = pp_pat pat indent false
       in
         patrn
         ^ " => "
         ^ pp_exp exp env (incr_indent indent (String.size patrn + 6))
       end

   (* DECLARATIONS *)
   and pp_dec dec env indent =
       let
         fun pp_dec' dec indent =
             case dec of
                 ValDec (vbs, tyvars) =>
                 "val "
                 ^ pp_seq (newline indent ^ "and ") (fn v => ppVb v env indent) vbs
               | ValrecDec (rvbs, tyvars) =>
                 "val rec "
                 ^ pp_seq (newline indent ^ "and ") (fn r => ppRvb r env indent) rvbs
               | DoDec exp =>
                 "do "
                 ^ pp_exp exp env indent
               | FunDec (fbs, tyvars) =>
                 "fun "
                 ^ pp_seq (newline indent ^ "and ") (fn f => ppFb f env indent) fbs
               | TypeDec tycs =>
                 "type "
                 ^ pp_seq " " (fn t => ppTb t indent) tycs
               | DatatypeDec {datatycs, withtycs=[]} =>
                 "datatype "
                 ^ pp_seq " " (fn d => ppDb d indent) datatycs
               | DatatypeDec {datatycs, withtycs} =>
                 "datatype "
                 ^ pp_seq " " (fn d => ppDb d indent) datatycs
                 ^ newline indent
                 ^ "withtype "
                 ^ pp_seq " " (fn t => ppTb t indent) withtycs
               | AbstypeDec {abstycs, withtycs=[], body} =>
                 "datatype "
                 ^ pp_seq " " (fn d => ppDb d indent) abstycs
                 ^ newline indent
                 ^ pp_dec' body indent
               | AbstypeDec {abstycs, withtycs, body} =>
                 "datatype "
                 ^ pp_seq " " (fn d => ppDb d indent) abstycs
                 ^ newline indent
                 ^ "withtype "
                 ^ pp_seq " " (fn t => ppTb t indent) withtycs
                 ^ newline indent
                 ^ pp_dec' body indent
               | ExceptionDec ebs => pp_seq " " (fn p => ppEb p indent) ebs
               | StrDec sbs => "structure "
                               ^ pp_seq " " (fn s => ppStrb s env indent) sbs
               | AbsDec sbs => "<abstraction?>"
               | FctDec fbs => "<functor>"
               | SigDec sigvars =>
                 let
                   fun pp_sig (Sigb{name, def}) =
                       pp_sym name
                       ^ " = "
                       ^ ppSigExp def env indent
                     | pp_sig (MarkSigb (s, n)) = pp_sig s
                 in
                   "signature "
                   ^ pp_seq " and " pp_sig sigvars
                 end
               | FsigDec sigvars => "<functor signature>"
               | LocalDec (inner, outer) =>
                 "local"
                 ^ "\n"
                 ^ pp_dec' inner (indent ^ "  ")
                 ^ newline indent
                 ^ "in"
                 ^ "\n"
                 ^ pp_dec' outer (indent ^ "  ")
                 ^ newline indent
                 ^ "end"
               | SeqDec decs =>
                 pp_het_seq (fn FunDec _ => true
                              | MarkDec (FunDec _, _) => true
                              | _ => false)
                            (newline indent, newline indent ^ newline indent)
                            (fn d => pp_dec' d indent)
                            decs
                 ^ "\n"
               | OpenDec strbs =>
                 "open "
                 ^ pp_seq " " pp_symbol_list strbs
               | OvldDec (sym, ty, explist) => pp_sym sym
               | FixDec {fixity, ops} =>
                 (case fixity of
                        NONfix => "nonfix"
                      | INfix (i, _) =>
                        (if i mod 2 = 0
                         then "infix "
                         else "infixr ")
                        ^ (if i div 2 > 0
                           then Int.toString (i div 2)
                           else "")
                        ^ pp_seq " " pp_sym ops)
               | DataReplDec (sym, path) => "datatype "
                                            ^ pp_sym sym
                                            ^ " = "
                                            ^ "datatype "
                                            ^ pp_symbol_list path
               | MarkDec (dec, (s, e)) => pp_dec' dec indent
       in
         pp_dec' dec indent
       end

   (* BINDINGS *)
   and ppVb vb env indent =
       case vb of
           Vb {pat, exp, ...} =>
           let
             val patrn = pp_pat pat indent false
             val size = String.size patrn
           in
             patrn
             ^ " = "
             ^ (let
                 fun exp_p n_ind = pp_exp exp env n_ind
                 val exp_ind = exp_p (incr_indent indent (size + 2))
               in
                 if String.size exp_ind + size + 2 > 80
                 then newline (indent ^ "  ") ^ (exp_p (indent ^ "  "))
                 else exp_ind
               end)
           end
         | MarkVb (vb, region) => ppVb vb env indent

   and ppRvb rvb env indent =
       case rvb of
           Rvb {var, exp, ...} =>
           pp_sym var
           ^ " = "
           ^ pp_exp exp env indent
         | MarkRvb (rvb, region) => ppRvb rvb env indent

   and ppFb fb env indent =
       case fb of
           Fb (clauses, ops) =>
           pp_seq (newline indent ^ "  | ") (fn c => ppClause c env (indent ^ "  ")) clauses
         | MarkFb (fb, region) => ppFb fb env indent

   and ppClause cls env indent =
       case cls of
           Clause {pats, resultty, exp} =>
           let
             fun ppCls {item, fixity, region} = pp_pat item indent true
           in
             pp_seq " " ppCls pats
             ^ (case resultty of
                    SOME ty => " : " ^ pp_typ ty indent
                  | NONE => "")
             ^ " = "
             ^ (let
                 val exp = pp_exp exp env (indent ^ "  ")
               in
                 if String.size exp > 20
                 then newline (indent ^ "  ") ^ exp
                 else exp
               end)
           end

   and ppTb tb indent =
       let
         fun pp_tyvar_list symbol_list =
             pp_seq "* " ppTyvar symbol_list
       in
         case tb of
             Tb {tyc, def, tyvars} => pp_sym tyc
                                      ^  " = "
                                      ^ pp_typ def indent
                                      ^ pp_tyvar_list tyvars
           | MarkTb (t, r) => ppTb t indent
       end

   and ppDb db indent =
       let
         fun pp_tyvar_list symbol_list =
             pp_seq ", " ppTyvar symbol_list
       in
         case db of
             Db {tyc, tyvars, rhs, lazyp} => pp_tyvar_list tyvars
                                             ^ pp_sym tyc
                                             ^  " = "
                                             ^ ppDbrhs rhs indent
           | MarkDb (t, r) => ppDb t indent
       end

   and ppDbrhs rhs indent =
       let
         fun pprhs (sym, tv) =
             case tv of
                 SOME a => pp_sym sym
                           ^ " of "
                           ^ pp_typ a indent
               | NONE => pp_sym sym
       in
         pp_seq (newline indent ^ " | ") pprhs rhs
       end

   and ppEb eb indent =
       case eb of
           EbGen {exn, etype} =>
           (case etype of
                SOME a => pp_sym exn
                          ^ " = "
                          ^ pp_typ a indent
              | NONE => pp_sym exn)
         | EbDef {exn, edef} =>
           pp_sym exn
           ^ " = "
           ^ pp_symbol_list edef
         | MarkEb (t, r) => ppEb t indent

   and ppStrb str env indent =
       case str of
           Strb {constraint, def, name} =>
           pp_sym name
           ^ (case constraint of
                  NoSig => ""
                | Transparent sigexp => " : "
                                        ^ ppSigExp sigexp env indent
                | Opaque sigexp => " :> "
                                   ^ ppSigExp sigexp env indent)
           ^ " = "
           ^ ppStrExp def env indent
         | MarkStrb (t, r) => ppStrb t env indent

   and ppStrExp str env indent =
       case str of
           VarStr p => pp_symbol_list p
         | BaseStr (SeqDec []) => "struct end"
         | BaseStr de => newline indent
                         ^ "struct"
                         ^ newline (indent ^ "  ")
                         ^ pp_dec de env (indent ^ "  ")
                         ^ newline indent
                         ^ "end"
         | ConstrainedStr (stre, constraint) =>
           ppStrExp stre env indent
           ^ (case constraint of
                  NoSig => ""
                | Transparent sigexp => " : "
                                        ^ ppSigExp sigexp env indent
                | Opaque sigexp => " :> "
                                   ^ ppSigExp sigexp env indent)
         | AppStr (path, str_list) =>
           pp_symbol_list path
           ^ pp_seq " " (fn (strl, _) => "(" ^ ppStrExp strl env indent ^ ")") str_list
         | AppStrI (path, str_list) =>
           pp_symbol_list path
           ^ pp_seq " " (fn (strl, _) => "(" ^ ppStrExp strl env indent ^ ")") str_list
         | LetStr (dec, body) =>
           "let\n"
           ^ pp_dec dec env indent
           ^ "\n" ^ "in\n"
           ^ ppStrExp body env indent
           ^ "\n" ^ "end"
         | MarkStr (body, (s, e)) => ppStrExp body env indent


   and ppSigExp exp env indent =
       case exp of
           VarSig s => pp_sym s
         | AugSig (sign, wherel) =>
           ppSigExp sign env indent
           ^ (case sign of
                  VarSig s => ""
                | MarkSig (VarSig s, _) => ""
                | _ => newline indent)
           ^ "where "
           ^ pp_seq " and " (fn w => ppWhereSpec w env indent) wherel

         | BaseSig [] => "sig end"
         | BaseSig specl => "sig"
                           ^ newline (indent ^ "  ")
                           ^ pp_seq (newline (indent ^ "  ")) (fn s => ppSpec s env (indent ^ "  ")) specl
                           ^ newline indent
                           ^ "end"
         | MarkSig (m, r) => ppSigExp m env indent

   and ppWhereSpec _ _ _ = "<whereSpec>"
   and ppSpec spec env indent =
       let
         fun pp_tyvar_list [] = ""
           | pp_tyvar_list [ty] = ppTyvar ty
           | pp_tyvar_list tyvar_list =
             "(" ^ pp_seq ", " ppTyvar tyvar_list ^ ")"
       in
         case spec of
             StrSpec sspo_list =>
             let
               fun pp (symbol, sigexp, path) =
                   pp_sym symbol
                   ^ " = "
                   ^ ppSigExp sigexp env indent
                   ^ (case path of
                          SOME p => " " ^ pp_symbol_list p
                        | NONE => "")
             in
               "structure "
               ^ pp_seq ", " pp sspo_list
             end
           | TycSpec (stto_list, b) =>
             let
               fun pp (symbol, tyvar_list, tyo) =
                   pp_tyvar_list tyvar_list
                   ^ pp_sym symbol
                   ^ (case tyo of
                          SOME ty => " = " ^ pp_typ ty indent
                        | NONE => "")
             in
               "type "
               ^ pp_seq ("|" ^ newline (indent ^ "    ")) pp stto_list
             end
           | FctSpec sf_list =>
             let
               fun pp (symbol, fsigexp) = pp_sym symbol
                                          ^ " : "
                                          ^ ppFsigExp fsigexp
             in
               "functor "
               ^ pp_seq (newline indent ^ "and ") pp sf_list
             end
           | ValSpec st_list =>
             let
               fun pp (symbol, ty) = "val "
                                     ^ pp_sym symbol
                                     ^ " : "
                                     ^ pp_typ ty indent
             in
               pp_seq (newline indent ^ "and ") pp st_list
             end
           | DataReplSpec (name, path) =>
             "datatype "
             ^ pp_sym name
             ^ " = datatype"
             ^ pp_symbol_list path
           | DataSpec {datatycs, withtycs=[]} =>
             "datatype "
             ^ pp_seq (newline indent ^ "and ") (fn d => ppDb d indent) datatycs
           | DataSpec {datatycs, withtycs} =>
             "datatype "
             ^ pp_seq (newline indent ^ "and ") (fn d => ppDb d indent) datatycs
             ^ newline indent
             ^ "withtype "
             ^ pp_seq (newline indent ^ "and ") (fn t => ppTb t indent) withtycs
           | ExceSpec sto_list =>
             let
               fun pp (symbol, tyo) =
                   case tyo of
                       SOME ty => pp_sym symbol ^ " of " ^ pp_typ ty indent
                     | NONE => pp_sym symbol
             in
               "exception "
               ^ pp_seq (newline indent ^ "and ") pp sto_list
             end
           | ShareStrSpec paths =>
             "sharing "
             ^ pp_seq " = " pp_symbol_list paths
           | ShareTycSpec paths =>
             "sharing type "
             ^ pp_seq " = " pp_symbol_list paths
           | IncludeSpec sigexp => ppSigExp sigexp env indent
           | MarkSpec (m, r) => ppSpec m env indent
       end

   and ppFsigExp _ = "<functor sig>"
end
