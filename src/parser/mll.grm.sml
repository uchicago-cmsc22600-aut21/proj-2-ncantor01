structure MLLTokens =
  struct
    datatype token
      = KW_case
      | KW_data
      | KW_else
      | KW_end
      | KW_fun
      | KW_if
      | KW_let
      | KW_of
      | KW_then
      | LP
      | RP
      | LB
      | RB
      | LCB
      | RCB
      | ASSIGN
      | ORELSE
      | ANDALSO
      | EQEQ
      | NEQ
      | LTEQ
      | LT
      | CONS
      | CONCAT
      | PLUS
      | MINUS
      | TIMES
      | DIV
      | MOD
      | DEREF
      | EQ
      | COMMA
      | SEMI
      | BAR
      | ARROW
      | DARROW
      | WILD
      | UID of Atom.atom
      | LID of Atom.atom
      | NUMBER of IntInf.int
      | STRING of string
      | EOF
    val allToks = [
            KW_case, KW_data, KW_else, KW_end, KW_fun, KW_if, KW_let, KW_of, KW_then, LP, RP, LB, RB, LCB, RCB, ASSIGN, ORELSE, ANDALSO, EQEQ, NEQ, LTEQ, LT, CONS, CONCAT, PLUS, MINUS, TIMES, DIV, MOD, DEREF, EQ, COMMA, SEMI, BAR, ARROW, DARROW, WILD, EOF
           ]
    fun toString tok =
(case (tok)
 of (KW_case) => "case"
  | (KW_data) => "data"
  | (KW_else) => "else"
  | (KW_end) => "end"
  | (KW_fun) => "fun"
  | (KW_if) => "if"
  | (KW_let) => "let"
  | (KW_of) => "of"
  | (KW_then) => "then"
  | (LP) => "("
  | (RP) => ")"
  | (LB) => "["
  | (RB) => "]"
  | (LCB) => "{"
  | (RCB) => "}"
  | (ASSIGN) => ":="
  | (ORELSE) => "||"
  | (ANDALSO) => "&&"
  | (EQEQ) => "=="
  | (NEQ) => "!="
  | (LTEQ) => "<="
  | (LT) => "<"
  | (CONS) => "::"
  | (CONCAT) => "^"
  | (PLUS) => "+"
  | (MINUS) => "-"
  | (TIMES) => "*"
  | (DIV) => "/"
  | (MOD) => "%"
  | (DEREF) => "!"
  | (EQ) => "="
  | (COMMA) => ","
  | (SEMI) => ";"
  | (BAR) => "|"
  | (ARROW) => "->"
  | (DARROW) => "=>"
  | (WILD) => "_"
  | (UID(_)) => "UID"
  | (LID(_)) => "LID"
  | (NUMBER(_)) => "NUMBER"
  | (STRING(_)) => "STRING"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (KW_case) => false
  | (KW_data) => false
  | (KW_else) => false
  | (KW_end) => false
  | (KW_fun) => false
  | (KW_if) => false
  | (KW_let) => false
  | (KW_of) => false
  | (KW_then) => false
  | (LP) => false
  | (RP) => false
  | (LB) => false
  | (RB) => false
  | (LCB) => false
  | (RCB) => false
  | (ASSIGN) => false
  | (ORELSE) => false
  | (ANDALSO) => false
  | (EQEQ) => false
  | (NEQ) => false
  | (LTEQ) => false
  | (LT) => false
  | (CONS) => false
  | (CONCAT) => false
  | (PLUS) => false
  | (MINUS) => false
  | (TIMES) => false
  | (DIV) => false
  | (MOD) => false
  | (DEREF) => false
  | (EQ) => false
  | (COMMA) => false
  | (SEMI) => false
  | (BAR) => false
  | (ARROW) => false
  | (DARROW) => false
  | (WILD) => false
  | (UID(_)) => false
  | (LID(_)) => false
  | (NUMBER(_)) => false
  | (STRING(_)) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* MLLTokens *)

functor MLLParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
MLLTokens
    structure UserCode =
      struct

  structure PT = ParseTree
  structure Op = OpNames

  type pos = Error.pos


fun TopDcl_PROD_1_SUBRULE_2_PROD_1_ACT (EQ, BAR, TyParams, KW_data, ConDcl, UID, EQ_SPAN : (Lex.pos * Lex.pos), BAR_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), KW_data_SPAN : (Lex.pos * Lex.pos), ConDcl_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ConDcl )
fun TopDcl_PROD_1_ACT (EQ, SR, TyParams, KW_data, ConDcl, UID, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), KW_data_SPAN : (Lex.pos * Lex.pos), ConDcl_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case TyParams of
                SOME a => ( PT.DclData( UID, a, List.concat( [ConDcl :: [], SR] ) ) )
                | NONE => ( PT.DclData( UID, [], List.concat( [ConDcl :: [], SR] ) ) ) )
fun TyParams_PROD_1_SUBRULE_1_PROD_1_ACT (LB, LID, COMMA, LB_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( LID )
fun TyParams_PROD_1_ACT (LB, SR, RB, LID, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( List.concat ([LID :: [], SR]) )
fun ConDcl_PROD_1_ACT (SR, UID, SR_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.Con( UID, SR ) )
fun Type_PROD_1_SUBRULE_1_PROD_1_ACT (Type, ARROW, Type_SPAN : (Lex.pos * Lex.pos), ARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Type)
fun Type_PROD_1_ACT (SR, Type, SR_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( list.foldl ( fn ( left, right ) => PT.TyFun ( left, right ) ) Type SR  )
fun Type_PROD_2_SUBRULE_1_PROD_1_ACT (TIMES, AtomicType, TIMES_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AtomicType )
fun Type_PROD_2_ACT (SR, AtomicType, SR_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.TyTuple ( List.concat( [AtomicType :: [], SR] ) ) )
fun AtomicType_PROD_1_ACT (TyArgs, UID, TyArgs_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case TyArgs of
                SOME a => ( PT.TyCon( UID, a ) )
                | NONE => ( PT.TyVar( UID ) ) )
fun AtomicType_PROD_2_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.TyVar( LID ) )
fun AtomicType_PROD_3_ACT (LP, RP, Type, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Type )
fun TyArgs_PROD_1_SUBRULE_1_PROD_1_ACT (LB, Type, COMMA, LB_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Type )
fun TyArgs_PROD_1_ACT (LB, SR, RB, Type, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ()
fun ValBind_PROD_1_ACT (EQ, KW_let, AtomicPat, Exp, EQ_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), AtomicPat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ()
fun ValBind_PROD_2_ACT (EQ, KW_fun, LID, AtomicPat, Exp, EQ_SPAN : (Lex.pos * Lex.pos), KW_fun_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), AtomicPat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ()
fun Pat_PROD_1_ACT (SimplePat, UID, SimplePat_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ()
fun Pat_PROD_2_ACT (CONS, SimplePat1, SimplePat2, CONS_SPAN : (Lex.pos * Lex.pos), SimplePat1_SPAN : (Lex.pos * Lex.pos), SimplePat2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ()
fun Pat_PROD_3_ACT (AtomicPat, AtomicPat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ()
fun AtomicPat_PROD_1_SUBRULE_1_PROD_1_ACT (LP, SimplePat, COMMA, LP_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( SimplePat )
fun AtomicPat_PROD_1_ACT (LP, SR, RP, SimplePat, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ()
fun SimplePat_PROD_1_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ()
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchKW_case strm = (case (lex(strm))
 of (Tok.KW_case, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_data strm = (case (lex(strm))
 of (Tok.KW_data, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_else strm = (case (lex(strm))
 of (Tok.KW_else, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_end strm = (case (lex(strm))
 of (Tok.KW_end, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_fun strm = (case (lex(strm))
 of (Tok.KW_fun, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_if strm = (case (lex(strm))
 of (Tok.KW_if, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_let strm = (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_of strm = (case (lex(strm))
 of (Tok.KW_of, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_then strm = (case (lex(strm))
 of (Tok.KW_then, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLB strm = (case (lex(strm))
 of (Tok.LB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRB strm = (case (lex(strm))
 of (Tok.RB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLCB strm = (case (lex(strm))
 of (Tok.LCB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRCB strm = (case (lex(strm))
 of (Tok.RCB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchASSIGN strm = (case (lex(strm))
 of (Tok.ASSIGN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchORELSE strm = (case (lex(strm))
 of (Tok.ORELSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchANDALSO strm = (case (lex(strm))
 of (Tok.ANDALSO, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQEQ strm = (case (lex(strm))
 of (Tok.EQEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNEQ strm = (case (lex(strm))
 of (Tok.NEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLTEQ strm = (case (lex(strm))
 of (Tok.LTEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLT strm = (case (lex(strm))
 of (Tok.LT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCONS strm = (case (lex(strm))
 of (Tok.CONS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCONCAT strm = (case (lex(strm))
 of (Tok.CONCAT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPLUS strm = (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMINUS strm = (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIMES strm = (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDIV strm = (case (lex(strm))
 of (Tok.DIV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMOD strm = (case (lex(strm))
 of (Tok.MOD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDEREF strm = (case (lex(strm))
 of (Tok.DEREF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQ strm = (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBAR strm = (case (lex(strm))
 of (Tok.BAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchARROW strm = (case (lex(strm))
 of (Tok.ARROW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDARROW strm = (case (lex(strm))
 of (Tok.DARROW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchWILD strm = (case (lex(strm))
 of (Tok.WILD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchUID strm = (case (lex(strm))
 of (Tok.UID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchLID strm = (case (lex(strm))
 of (Tok.LID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNUMBER strm = (case (lex(strm))
 of (Tok.NUMBER(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (Program_NT) = 
let
fun Exp_NT (strm) = let
      val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
      val FULL_SPAN = (#1(KW_if_SPAN), #2(KW_if_SPAN))
      in
        ((), FULL_SPAN, strm')
      end
fun SimplePat_NT (strm) = let
      val (LID_RES, LID_SPAN, strm') = matchLID(strm)
      val FULL_SPAN = (#1(LID_SPAN), #2(LID_SPAN))
      in
        (UserCode.SimplePat_PROD_1_ACT (LID_RES, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun AtomicPat_NT (strm) = let
      fun AtomicPat_PROD_1 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (SimplePat_RES, SimplePat_SPAN, strm') = SimplePat_NT(strm')
            fun AtomicPat_PROD_1_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (SimplePat_RES, SimplePat_SPAN, strm') = SimplePat_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(SimplePat_SPAN))
                  in
                    (UserCode.AtomicPat_PROD_1_SUBRULE_1_PROD_1_ACT (LP_RES, SimplePat_RES, COMMA_RES, LP_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun AtomicPat_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomicPat_PROD_1_SUBRULE_1_PRED, AtomicPat_PROD_1_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomicPat_PROD_1_ACT (LP_RES, SR_RES, RP_RES, SimplePat_RES, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicPat_PROD_2 (strm) = let
            val (SimplePat_RES, SimplePat_SPAN, strm') = SimplePat_NT(strm)
            val FULL_SPAN = (#1(SimplePat_SPAN), #2(SimplePat_SPAN))
            in
              ((SimplePat_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LID(_), _, strm') => AtomicPat_PROD_2(strm)
          | (Tok.LP, _, strm') => AtomicPat_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun ValBind_NT (strm) = let
      fun ValBind_PROD_1 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (AtomicPat_RES, AtomicPat_SPAN, strm') = AtomicPat_NT(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(Exp_SPAN))
            in
              (UserCode.ValBind_PROD_1_ACT (EQ_RES, KW_let_RES, AtomicPat_RES, Exp_RES, EQ_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), AtomicPat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValBind_PROD_2 (strm) = let
            val (KW_fun_RES, KW_fun_SPAN, strm') = matchKW_fun(strm)
            val (LID_RES, LID_SPAN, strm') = matchLID(strm')
            fun ValBind_PROD_2_SUBRULE_1_NT (strm) = let
                  val (AtomicPat_RES, AtomicPat_SPAN, strm') = AtomicPat_NT(strm)
                  val FULL_SPAN = (#1(AtomicPat_SPAN), #2(AtomicPat_SPAN))
                  in
                    ((AtomicPat_RES), FULL_SPAN, strm')
                  end
            fun ValBind_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LP, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (AtomicPat_RES, AtomicPat_SPAN, strm') = EBNF.posclos(ValBind_PROD_2_SUBRULE_1_PRED, ValBind_PROD_2_SUBRULE_1_NT, strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val FULL_SPAN = (#1(KW_fun_SPAN), #2(Exp_SPAN))
            in
              (UserCode.ValBind_PROD_2_ACT (EQ_RES, KW_fun_RES, LID_RES, AtomicPat_RES, Exp_RES, EQ_SPAN : (Lex.pos * Lex.pos), KW_fun_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), AtomicPat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') => ValBind_PROD_2(strm)
          | (Tok.KW_let, _, strm') => ValBind_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Type_NT (strm) = let
      fun Type_PROD_1 (strm) = let
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm)
            fun Type_PROD_1_SUBRULE_1_NT (strm) = let
                  val (ARROW_RES, ARROW_SPAN, strm') = matchARROW(strm)
                  val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
                  val FULL_SPAN = (#1(ARROW_SPAN), #2(Type_SPAN))
                  in
                    (UserCode.Type_PROD_1_SUBRULE_1_PROD_1_ACT (Type_RES, ARROW_RES, Type_SPAN : (Lex.pos * Lex.pos), ARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun Type_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.ARROW, _, strm') =>
                        (case (lex(strm'))
                         of _ => false
                        (* end case *))
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Type_PROD_1_SUBRULE_1_PRED, Type_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Type_SPAN), #2(SR_SPAN))
            in
              (UserCode.Type_PROD_1_ACT (SR_RES, Type_RES, SR_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_2 (strm) = let
            val (AtomicType_RES, AtomicType_SPAN, strm') = AtomicType_NT(strm)
            fun Type_PROD_2_SUBRULE_1_NT (strm) = let
                  val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
                  val (AtomicType_RES, AtomicType_SPAN, strm') = AtomicType_NT(strm')
                  val FULL_SPAN = (#1(TIMES_SPAN), #2(AtomicType_SPAN))
                  in
                    (UserCode.Type_PROD_2_SUBRULE_1_PROD_1_ACT (TIMES_RES, AtomicType_RES, TIMES_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun Type_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.TIMES, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Type_PROD_2_SUBRULE_1_PRED, Type_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(AtomicType_SPAN), #2(SR_SPAN))
            in
              (UserCode.Type_PROD_2_ACT (SR_RES, AtomicType_RES, SR_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => Type_PROD_2(strm)
          | (Tok.UID(_), _, strm') => Type_PROD_2(strm)
          | (Tok.LID(_), _, strm') => Type_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and AtomicType_NT (strm) = let
      fun AtomicType_PROD_1 (strm) = let
            val (UID_RES, UID_SPAN, strm') = matchUID(strm)
            fun AtomicType_PROD_1_SUBRULE_1_NT (strm) = let
                  val (TyArgs_RES, TyArgs_SPAN, strm') = TyArgs_NT(strm)
                  val FULL_SPAN = (#1(TyArgs_SPAN), #2(TyArgs_SPAN))
                  in
                    ((TyArgs_RES), FULL_SPAN, strm')
                  end
            fun AtomicType_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LB, _, strm') => true
                    | _ => false
                  (* end case *))
            val (TyArgs_RES, TyArgs_SPAN, strm') = EBNF.optional(AtomicType_PROD_1_SUBRULE_1_PRED, AtomicType_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(UID_SPAN), #2(TyArgs_SPAN))
            in
              (UserCode.AtomicType_PROD_1_ACT (TyArgs_RES, UID_RES, TyArgs_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicType_PROD_2 (strm) = let
            val (LID_RES, LID_SPAN, strm') = matchLID(strm)
            val FULL_SPAN = (#1(LID_SPAN), #2(LID_SPAN))
            in
              (UserCode.AtomicType_PROD_2_ACT (LID_RES, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicType_PROD_3 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomicType_PROD_3_ACT (LP_RES, RP_RES, Type_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => AtomicType_PROD_3(strm)
          | (Tok.UID(_), _, strm') => AtomicType_PROD_1(strm)
          | (Tok.LID(_), _, strm') => AtomicType_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and TyArgs_NT (strm) = let
      val (LB_RES, LB_SPAN, strm') = matchLB(strm)
      val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
      fun TyArgs_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(Type_SPAN))
            in
              (UserCode.TyArgs_PROD_1_SUBRULE_1_PROD_1_ACT (LB_RES, Type_RES, COMMA_RES, LB_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyArgs_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(TyArgs_PROD_1_SUBRULE_1_PRED, TyArgs_PROD_1_SUBRULE_1_NT, strm')
      val (RB_RES, RB_SPAN, strm') = matchRB(strm')
      val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
      in
        (UserCode.TyArgs_PROD_1_ACT (LB_RES, SR_RES, RB_RES, Type_RES, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun ConDcl_NT (strm) = let
      val (UID_RES, UID_SPAN, strm') = matchUID(strm)
      fun ConDcl_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_of_RES, KW_of_SPAN, strm') = matchKW_of(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(KW_of_SPAN), #2(Type_SPAN))
            in
              ((Type_RES), FULL_SPAN, strm')
            end
      fun ConDcl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_of, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(ConDcl_PROD_1_SUBRULE_1_PRED, ConDcl_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(UID_SPAN), #2(SR_SPAN))
      in
        (UserCode.ConDcl_PROD_1_ACT (SR_RES, UID_RES, SR_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun TyParams_NT (strm) = let
      val (LB_RES, LB_SPAN, strm') = matchLB(strm)
      val (LID_RES, LID_SPAN, strm') = matchLID(strm')
      fun TyParams_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (LID_RES, LID_SPAN, strm') = matchLID(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(LID_SPAN))
            in
              (UserCode.TyParams_PROD_1_SUBRULE_1_PROD_1_ACT (LB_RES, LID_RES, COMMA_RES, LB_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyParams_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(TyParams_PROD_1_SUBRULE_1_PRED, TyParams_PROD_1_SUBRULE_1_NT, strm')
      val (RB_RES, RB_SPAN, strm') = matchRB(strm')
      val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
      in
        (UserCode.TyParams_PROD_1_ACT (LB_RES, SR_RES, RB_RES, LID_RES, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun TopDcl_NT (strm) = let
      fun TopDcl_PROD_1 (strm) = let
            val (KW_data_RES, KW_data_SPAN, strm') = matchKW_data(strm)
            val (UID_RES, UID_SPAN, strm') = matchUID(strm')
            fun TopDcl_PROD_1_SUBRULE_1_NT (strm) = let
                  val (TyParams_RES, TyParams_SPAN, strm') = TyParams_NT(strm)
                  val FULL_SPAN = (#1(TyParams_SPAN), #2(TyParams_SPAN))
                  in
                    ((TyParams_RES), FULL_SPAN, strm')
                  end
            fun TopDcl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LB, _, strm') => true
                    | _ => false
                  (* end case *))
            val (TyParams_RES, TyParams_SPAN, strm') = EBNF.optional(TopDcl_PROD_1_SUBRULE_1_PRED, TopDcl_PROD_1_SUBRULE_1_NT, strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (ConDcl_RES, ConDcl_SPAN, strm') = ConDcl_NT(strm')
            fun TopDcl_PROD_1_SUBRULE_2_NT (strm) = let
                  val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
                  val (ConDcl_RES, ConDcl_SPAN, strm') = ConDcl_NT(strm')
                  val FULL_SPAN = (#1(BAR_SPAN), #2(ConDcl_SPAN))
                  in
                    (UserCode.TopDcl_PROD_1_SUBRULE_2_PROD_1_ACT (EQ_RES, BAR_RES, TyParams_RES, KW_data_RES, ConDcl_RES, UID_RES, EQ_SPAN : (Lex.pos * Lex.pos), BAR_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), KW_data_SPAN : (Lex.pos * Lex.pos), ConDcl_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun TopDcl_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.BAR, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(TopDcl_PROD_1_SUBRULE_2_PRED, TopDcl_PROD_1_SUBRULE_2_NT, strm')
            val FULL_SPAN = (#1(KW_data_SPAN), #2(SR_SPAN))
            in
              (UserCode.TopDcl_PROD_1_ACT (EQ_RES, SR_RES, TyParams_RES, KW_data_RES, ConDcl_RES, UID_RES, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), KW_data_SPAN : (Lex.pos * Lex.pos), ConDcl_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TopDcl_PROD_2 (strm) = let
            val (ValBind_RES, ValBind_SPAN, strm') = ValBind_NT(strm)
            val FULL_SPAN = (#1(ValBind_SPAN), #2(ValBind_SPAN))
            in
              ((ValBind_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') => TopDcl_PROD_2(strm)
          | (Tok.KW_let, _, strm') => TopDcl_PROD_2(strm)
          | (Tok.KW_data, _, strm') => TopDcl_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Program_NT (strm) = let
      fun Program_PROD_1 (strm) = let
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm)
            val FULL_SPAN = (#1(Exp_SPAN), #2(Exp_SPAN))
            in
              ((Exp_RES), FULL_SPAN, strm')
            end
      fun Program_PROD_2 (strm) = let
            val (SR_RES, SR_SPAN, strm') = let
            fun Program_PROD_2_SUBRULE_1_NT (strm) = let
                  fun Program_PROD_2_SUBRULE_1_PROD_1 (strm) = let
                        val (TopDcl_RES, TopDcl_SPAN, strm') = TopDcl_NT(strm)
                        val FULL_SPAN = (#1(TopDcl_SPAN), #2(TopDcl_SPAN))
                        in
                          ((TopDcl_RES), FULL_SPAN, strm')
                        end
                  fun Program_PROD_2_SUBRULE_1_PROD_2 (strm) = let
                        val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm)
                        val FULL_SPAN = (#1(Exp_SPAN), #2(Exp_SPAN))
                        in
                          ((Exp_RES), FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.KW_if, _, strm') =>
                          Program_PROD_2_SUBRULE_1_PROD_2(strm)
                      | (Tok.KW_data, _, strm') =>
                          Program_PROD_2_SUBRULE_1_PROD_1(strm)
                      | (Tok.KW_fun, _, strm') =>
                          Program_PROD_2_SUBRULE_1_PROD_1(strm)
                      | (Tok.KW_let, _, strm') =>
                          Program_PROD_2_SUBRULE_1_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                  end
            in
              Program_PROD_2_SUBRULE_1_NT(strm)
            end
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (Program_RES, Program_SPAN, strm') = Program_NT(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(Program_SPAN))
            in
              ((SR_RES, Program_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_data, _, strm') => Program_PROD_2(strm)
          | (Tok.KW_fun, _, strm') => Program_PROD_2(strm)
          | (Tok.KW_let, _, strm') => Program_PROD_2(strm)
          | (Tok.KW_if, _, strm') =>
              (case (lex(strm'))
               of (Tok.EOF, _, strm') => Program_PROD_1(strm)
                | (Tok.SEMI, _, strm') => Program_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
in
  (Program_NT)
end
val Program_NT =  fn s => unwrap (Err.launch (eh, lexFn, Program_NT , true) s)

in (Program_NT) end
  in
fun parse lexFn  s = let val (Program_NT) = mk lexFn in Program_NT s end

  end

end
