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


fun Program_PROD_1_ACT (SR, env, Exp, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR of
                        NONE => ( PT.ProgMark {span = FULL_SPAN, tree = ( PT.Prog (env, Exp) )} )
                       | (SOME a) => a )
fun Program_PROD_2_ACT (TopDcl, SEMI, env, Program, TopDcl_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Program_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Program )
fun TopDcl_PROD_1_SUBRULE_2_PROD_1_ACT (EQ, BAR, TyParams, KW_data, ConDcl, UID, EQ_SPAN : (Lex.pos * Lex.pos), BAR_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), KW_data_SPAN : (Lex.pos * Lex.pos), ConDcl_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ConDcl )
fun TopDcl_PROD_1_ACT (EQ, SR, TyParams, KW_data, ConDcl, UID, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), KW_data_SPAN : (Lex.pos * Lex.pos), ConDcl_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case TyParams of
                SOME a => ( PT.DclMark { span = FULL_SPAN, tree = PT.DclData( UID, a, ConDcl :: SR ) } ) 
                | NONE => ( PT.DclMark { span = FULL_SPAN, tree = PT.DclData( UID, [], ConDcl :: SR  ) } ) )
fun TopDcl_PROD_2_ACT (ValBind, ValBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.DclVal (ValBind))
fun TyParams_PROD_1_SUBRULE_1_PROD_1_ACT (LB, LID, COMMA, LB_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( LID )
fun TyParams_PROD_1_ACT (LB, SR, RB, LID, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( LID :: SR )
fun ConDcl_PROD_1_ACT (SR, UID, SR_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.ConMark{ span = FULL_SPAN, tree = PT.Con( UID, SR ) } )
fun Type_PROD_1_SUBRULE_1_PROD_1_ACT (AtomicTypeList, ARROW, AtomicTypeList_SPAN : (Lex.pos * Lex.pos), ARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AtomicTypeList)
fun Type_PROD_1_ACT (SR, AtomicTypeList, SR_SPAN : (Lex.pos * Lex.pos), AtomicTypeList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( case SR of
                [] => (AtomicTypeList)
                | _  => (PT.TyMark { span = FULL_SPAN, 
                                     tree =(List.foldl ( fn ( left, right ) 
                                            => PT.TyFun ( left, right ) ) AtomicTypeList SR  ) } ) )
fun AtomicTypeList_PROD_1_SUBRULE_1_PROD_1_ACT (TIMES, AtomicType, TIMES_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AtomicType )
fun AtomicTypeList_PROD_1_ACT (SR, AtomicType, SR_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( case SR of
                [] => ( PT.TyMark { span = FULL_SPAN, tree = AtomicType } )
              | _  => ( PT.TyMark { span = FULL_SPAN, tree = PT.TyTuple ( AtomicType :: SR ) }  ) )
fun AtomicType_PROD_1_ACT (TyArgs, UID, TyArgs_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case TyArgs of
                SOME a => ( PT.TyMark { span = FULL_SPAN, tree = PT.TyCon( UID, a ) } )
                | NONE => ( PT.TyMark { span = FULL_SPAN, tree = PT.TyVar( UID ) } ) )
fun AtomicType_PROD_2_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.TyMark { span = FULL_SPAN, tree = PT.TyVar( LID ) } )
fun AtomicType_PROD_3_ACT (LP, RP, Type, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Type )
fun TyArgs_PROD_1_SUBRULE_1_PROD_1_ACT (LB, Type, COMMA, LB_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Type )
fun TyArgs_PROD_1_ACT (LB, SR, RB, Type, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Type :: SR)
fun ValBind_PROD_1_ACT (EQ, KW_let, AtomicPat, Exp, EQ_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), AtomicPat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.BindMark { span = FULL_SPAN, tree = PT.BindVal ( AtomicPat, Exp ) } )
fun ValBind_PROD_2_ACT (EQ, KW_fun, LID, AtomicPat, Exp, EQ_SPAN : (Lex.pos * Lex.pos), KW_fun_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), AtomicPat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.BindMark { span = FULL_SPAN, tree = PT.BindFun ( LID, AtomicPat, Exp ) } )
fun Pat_PROD_1_ACT (SimplePat, UID, SimplePat_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.PatMark { span = FULL_SPAN, tree = PT.PatCon ( UID, SimplePat ) } )
fun Pat_PROD_2_ACT (CONS, SimplePat1, SimplePat2, CONS_SPAN : (Lex.pos * Lex.pos), SimplePat1_SPAN : (Lex.pos * Lex.pos), SimplePat2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.PatMark { span = FULL_SPAN, tree = PT.PatListCons ( SimplePat1, SimplePat2 ) } )
fun AtomicPat_PROD_1_SUBRULE_1_PROD_1_ACT (LP, SimplePat, COMMA, LP_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( SimplePat )
fun AtomicPat_PROD_1_ACT (LP, SR, RP, SimplePat, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.PatMark { span = FULL_SPAN, tree = PT.PatTuple ( SimplePat :: SR ) } )
fun SimplePat_PROD_1_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.PatMark { span = FULL_SPAN, tree = PT.PatVar LID } )
fun SimplePat_PROD_2_ACT (WILD, WILD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.PatMark { span = FULL_SPAN, tree = PT.PatWild } )
fun Exp_PROD_1_ACT (Exp1, Exp2, Exp3, KW_if, KW_else, KW_then, Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), Exp3_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.ExpMark  { span = FULL_SPAN, tree = PT.ExpIf ( Exp1, Exp2, Exp3 ) } )
fun Exp_PROD_2_SUBRULE_1_PROD_1_ACT (OpExp, ASSIGN, OpExp_SPAN : (Lex.pos * Lex.pos), ASSIGN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpExp)
fun Exp_PROD_2_ACT (SR, OpExp, SR_SPAN : (Lex.pos * Lex.pos), OpExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR of
                                            NONE => OpExp
                                            | (SOME a) => PT.ExpMark {span = FULL_SPAN, tree = PT.ExpBin (OpExp, Atom.atom ":=", a) } )
fun OpExp_PROD_1_SUBRULE_1_PROD_1_ACT (ORELSE, OpExp1, ORELSE_SPAN : (Lex.pos * Lex.pos), OpExp1_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ("||", OpExp1) )
fun OpExp_PROD_1_SUBRULE_1_PROD_2_ACT (ANDALSO, OpExp1, ANDALSO_SPAN : (Lex.pos * Lex.pos), OpExp1_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ("&&", OpExp1) )
fun OpExp_PROD_1_ACT (SR, OpExp1, SR_SPAN : (Lex.pos * Lex.pos), OpExp1_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.ExpMark { span = FULL_SPAN, 
                                    tree = ( List.foldl ( fn ( (op', left), right ) 
                                        => (case op' of
                                            "||" => PT.ExpMark { span = FULL_SPAN, tree = PT.ExpOrElse (right, left) }
                                          | "&&" => PT.ExpMark { span = FULL_SPAN, tree = PT.ExpAndAlso (right, left) }
                                          |  _   => PT.ExpInt 0)  )
                                        OpExp1 SR   ) } )
fun OpExp1_PROD_1_SUBRULE_1_PROD_1_ACT (EQEQ, OpExp2, EQEQ_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "==", OpExp2) )
fun OpExp1_PROD_1_SUBRULE_1_PROD_2_ACT (NEQ, OpExp2, NEQ_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "!=", OpExp2) )
fun OpExp1_PROD_1_SUBRULE_1_PROD_3_ACT (LT, OpExp2, LT_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "<" , OpExp2) )
fun OpExp1_PROD_1_SUBRULE_1_PROD_4_ACT (LTEQ, OpExp2, LTEQ_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "<=", OpExp2) )
fun OpExp1_PROD_1_ACT (SR, OpExp2, SR_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.ExpMark { span = FULL_SPAN, 
                                      tree = ( List.foldl ( fn ( (op', left) , right ) 
                        => PT.ExpMark { span = FULL_SPAN, tree = PT.ExpBin ( right, op', left ) } ) OpExp2 SR  ) } )
fun OpExp2_PROD_1_SUBRULE_1_PROD_1_ACT (CONS, OpExp3, CONS_SPAN : (Lex.pos * Lex.pos), OpExp3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( OpExp3 )
fun OpExp2_PROD_1_ACT (SR, OpExp3, SR_SPAN : (Lex.pos * Lex.pos), OpExp3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.ExpMark { span = FULL_SPAN, 
                                  tree = (List.foldr ( fn ( left, right)  
                    => PT.ExpMark { span = FULL_SPAN, tree = PT.ExpListCons ( right, left ) } ) )OpExp3 SR   } )
fun OpExp3_PROD_1_SUBRULE_1_PROD_1_ACT (CONCAT, OpExp4, CONCAT_SPAN : (Lex.pos * Lex.pos), OpExp4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "^" , OpExp4) )
fun OpExp3_PROD_1_SUBRULE_1_PROD_2_ACT (PLUS, OpExp4, PLUS_SPAN : (Lex.pos * Lex.pos), OpExp4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "+" , OpExp4) )
fun OpExp3_PROD_1_SUBRULE_1_PROD_3_ACT (MINUS, OpExp4, MINUS_SPAN : (Lex.pos * Lex.pos), OpExp4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "-" , OpExp4) )
fun OpExp3_PROD_1_ACT (SR, OpExp4, SR_SPAN : (Lex.pos * Lex.pos), OpExp4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.ExpMark { span = FULL_SPAN, 
                                      tree = ( List.foldl ( fn ( ( op', left ), right ) 
                        => PT.ExpMark {span = FULL_SPAN, tree = PT.ExpBin ( right, op', left ) } ) OpExp4 SR  ) } )
fun OpExp4_PROD_1_SUBRULE_1_PROD_1_ACT (TIMES, OpExp5, TIMES_SPAN : (Lex.pos * Lex.pos), OpExp5_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "*" , OpExp5) )
fun OpExp4_PROD_1_SUBRULE_1_PROD_2_ACT (DIV, OpExp5, DIV_SPAN : (Lex.pos * Lex.pos), OpExp5_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "/" , OpExp5) )
fun OpExp4_PROD_1_SUBRULE_1_PROD_3_ACT (MOD, OpExp5, MOD_SPAN : (Lex.pos * Lex.pos), OpExp5_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ( Atom.atom "%" , OpExp5) )
fun OpExp4_PROD_1_ACT (SR, OpExp5, SR_SPAN : (Lex.pos * Lex.pos), OpExp5_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.ExpMark { span = FULL_SPAN, 
                                     tree = ( List.foldl ( fn ( (op', left) , right ) 
                        => PT.ExpMark {span = FULL_SPAN, tree = PT.ExpBin ( right, op', left ) } ) OpExp5 SR  ) } )
fun OpExp5_PROD_1_ACT (DEREF, ApplyExp, DEREF_SPAN : (Lex.pos * Lex.pos), ApplyExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.ExpMark { span = FULL_SPAN, tree = PT.ExpUn ( Atom.atom "!", ApplyExp ) } )
fun OpExp5_PROD_2_ACT (ApplyExp, MINUS, ApplyExp_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.ExpMark { span = FULL_SPAN, tree = PT.ExpUn ( Atom.atom "-", ApplyExp ) } )
fun ApplyExp_PROD_1_ACT (SR, AtomicExp, SR_SPAN : (Lex.pos * Lex.pos), AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR of
                        [] => AtomicExp
                        | _ => ( PT.ExpMark { span = FULL_SPAN, 
                                              tree = List.foldl ( fn (left, right) 
                                    => PT.ExpMark {span = FULL_SPAN, tree = PT.ExpApp (right, left)} ) AtomicExp SR } ) )
fun AtomicExp_PROD_1_ACT (UID, UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.ExpCon UID)
fun AtomicExp_PROD_2_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.ExpVar LID)
fun AtomicExp_PROD_3_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.ExpStr STRING)
fun AtomicExp_PROD_4_ACT (NUMBER, NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.ExpInt NUMBER)
fun AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (LP, Exp, COMMA, LP_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Exp)
fun AtomicExp_PROD_5_SUBRULE_1_PROD_1_ACT (LP, SR, Exp, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Exp :: SR)
fun AtomicExp_PROD_5_ACT (LP, SR, RP, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( case SR of
                        NONE => PT.ExpMark { span = FULL_SPAN, tree = PT.ExpTuple ([]) }
                        | (SOME a) => 
                            (case (List.length (a)) of 
                                1   => PT.ExpMark { span = FULL_SPAN, tree = List.nth (a, 0)}
                                | _ => PT.ExpMark { span = FULL_SPAN, tree = PT.ExpTuple a } ) )
fun AtomicExp_PROD_6_ACT (RCB, LCB, Scope, RCB_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.ExpMark { span = FULL_SPAN, tree = PT.ExpScope Scope } )
fun AtomicExp_PROD_7_ACT (SR, KW_end, KW_of, Exp, KW_case, SR_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_of_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), KW_case_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PT.ExpMark { span = FULL_SPAN, tree = PT.ExpCase ( Exp, SR ) } )
fun Scope_PROD_1_ACT (SR, env, Exp, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( case SR of
                   NONE  => ( env, Exp ) 
                   | SOME a => a )
fun Scope_PROD_2_ACT (ValBind, SEMI, env, Scope, ValBind_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Scope )
fun MatchCase_PROD_1_ACT (Pat, RCB, LCB, DARROW, Scope, Pat_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.RuleMark { span = FULL_SPAN, tree = PT.RuleCase ( Pat, Scope ) } )
fun ARGS_3 () = 
  ([])
fun ARGS_5 (SEMI, env, Exp) = 
  ( List.concat ( [env,  [ ( PT.DclVal (PT.BindExp Exp) ) ] ] ) )
fun ARGS_7 (TopDcl, SEMI, env) = 
  (List.concat ([env, [TopDcl]]))
fun ARGS_63 (LCB) = 
  ([])
fun ARGS_66 (SEMI, env, Exp) = 
  ( List.concat ( [ env, [PT.BindExp Exp ] ] ) )
fun ARGS_68 (ValBind, SEMI, env) = 
  ( List.concat ( [ env, [ ValBind ] ] ) )
fun ARGS_70 (Pat, LCB, DARROW) = 
  ([])
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

val (PreProg_NT) = 
let
fun SimplePat_NT (strm) = let
      fun SimplePat_PROD_1 (strm) = let
            val (LID_RES, LID_SPAN, strm') = matchLID(strm)
            val FULL_SPAN = (#1(LID_SPAN), #2(LID_SPAN))
            in
              (UserCode.SimplePat_PROD_1_ACT (LID_RES, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SimplePat_PROD_2 (strm) = let
            val (WILD_RES, WILD_SPAN, strm') = matchWILD(strm)
            val FULL_SPAN = (#1(WILD_SPAN), #2(WILD_SPAN))
            in
              (UserCode.SimplePat_PROD_2_ACT (WILD_RES, WILD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.WILD, _, strm') => SimplePat_PROD_2(strm)
          | (Tok.LID(_), _, strm') => SimplePat_PROD_1(strm)
          | _ => fail()
        (* end case *))
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
         of (Tok.WILD, _, strm') => AtomicPat_PROD_2(strm)
          | (Tok.LID(_), _, strm') => AtomicPat_PROD_2(strm)
          | (Tok.LP, _, strm') => AtomicPat_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Pat_NT (strm) = let
      fun Pat_PROD_1 (strm) = let
            val (UID_RES, UID_SPAN, strm') = matchUID(strm)
            fun Pat_PROD_1_SUBRULE_1_NT (strm) = let
                  val (SimplePat_RES, SimplePat_SPAN, strm') = SimplePat_NT(strm)
                  val FULL_SPAN = (#1(SimplePat_SPAN), #2(SimplePat_SPAN))
                  in
                    ((SimplePat_RES), FULL_SPAN, strm')
                  end
            fun Pat_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.WILD, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SimplePat_RES, SimplePat_SPAN, strm') = EBNF.optional(Pat_PROD_1_SUBRULE_1_PRED, Pat_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(UID_SPAN), #2(SimplePat_SPAN))
            in
              (UserCode.Pat_PROD_1_ACT (SimplePat_RES, UID_RES, SimplePat_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Pat_PROD_2 (strm) = let
            val (SimplePat1_RES, SimplePat1_SPAN, strm') = SimplePat_NT(strm)
            val (CONS_RES, CONS_SPAN, strm') = matchCONS(strm')
            val (SimplePat2_RES, SimplePat2_SPAN, strm') = SimplePat_NT(strm')
            val FULL_SPAN = (#1(SimplePat1_SPAN), #2(SimplePat2_SPAN))
            in
              (UserCode.Pat_PROD_2_ACT (CONS_RES, SimplePat1_RES, SimplePat2_RES, CONS_SPAN : (Lex.pos * Lex.pos), SimplePat1_SPAN : (Lex.pos * Lex.pos), SimplePat2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Pat_PROD_3 (strm) = let
            val (AtomicPat_RES, AtomicPat_SPAN, strm') = AtomicPat_NT(strm)
            val FULL_SPAN = (#1(AtomicPat_SPAN), #2(AtomicPat_SPAN))
            in
              ((AtomicPat_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => Pat_PROD_3(strm)
          | (Tok.UID(_), _, strm') => Pat_PROD_1(strm)
          | (Tok.WILD, _, strm') =>
              (case (lex(strm'))
               of (Tok.CONS, _, strm') => Pat_PROD_2(strm)
                | (Tok.DARROW, _, strm') => Pat_PROD_3(strm)
                | _ => fail()
              (* end case *))
          | (Tok.LID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.CONS, _, strm') => Pat_PROD_2(strm)
                | (Tok.DARROW, _, strm') => Pat_PROD_3(strm)
                | _ => fail()
              (* end case *))
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
                    | (Tok.WILD, _, strm') => true
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
and Exp_NT (strm) = let
      fun Exp_PROD_1 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (Exp1_RES, Exp1_SPAN, strm') = Exp_NT(strm')
            val (KW_then_RES, KW_then_SPAN, strm') = matchKW_then(strm')
            val (Exp2_RES, Exp2_SPAN, strm') = Exp_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (Exp3_RES, Exp3_SPAN, strm') = Exp_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(Exp3_SPAN))
            in
              (UserCode.Exp_PROD_1_ACT (Exp1_RES, Exp2_RES, Exp3_RES, KW_if_RES, KW_else_RES, KW_then_RES, Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), Exp3_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Exp_PROD_2 (strm) = let
            val (OpExp_RES, OpExp_SPAN, strm') = OpExp_NT(strm)
            fun Exp_PROD_2_SUBRULE_1_NT (strm) = let
                  val (ASSIGN_RES, ASSIGN_SPAN, strm') = matchASSIGN(strm)
                  val (OpExp_RES, OpExp_SPAN, strm') = OpExp_NT(strm')
                  val FULL_SPAN = (#1(ASSIGN_SPAN), #2(OpExp_SPAN))
                  in
                    (UserCode.Exp_PROD_2_SUBRULE_1_PROD_1_ACT (OpExp_RES, ASSIGN_RES, OpExp_SPAN : (Lex.pos * Lex.pos), ASSIGN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun Exp_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.ASSIGN, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(Exp_PROD_2_SUBRULE_1_PRED, Exp_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(OpExp_SPAN), #2(SR_SPAN))
            in
              (UserCode.Exp_PROD_2_ACT (SR_RES, OpExp_RES, SR_SPAN : (Lex.pos * Lex.pos), OpExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_case, _, strm') => Exp_PROD_2(strm)
          | (Tok.LP, _, strm') => Exp_PROD_2(strm)
          | (Tok.LCB, _, strm') => Exp_PROD_2(strm)
          | (Tok.MINUS, _, strm') => Exp_PROD_2(strm)
          | (Tok.DEREF, _, strm') => Exp_PROD_2(strm)
          | (Tok.UID(_), _, strm') => Exp_PROD_2(strm)
          | (Tok.LID(_), _, strm') => Exp_PROD_2(strm)
          | (Tok.NUMBER(_), _, strm') => Exp_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => Exp_PROD_2(strm)
          | (Tok.KW_if, _, strm') => Exp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and OpExp_NT (strm) = let
      val (OpExp1_RES, OpExp1_SPAN, strm') = OpExp1_NT(strm)
      fun OpExp_PROD_1_SUBRULE_1_NT (strm) = let
            fun OpExp_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (ORELSE_RES, ORELSE_SPAN, strm') = matchORELSE(strm)
                  val (OpExp1_RES, OpExp1_SPAN, strm') = OpExp1_NT(strm')
                  val FULL_SPAN = (#1(ORELSE_SPAN), #2(OpExp1_SPAN))
                  in
                    (UserCode.OpExp_PROD_1_SUBRULE_1_PROD_1_ACT (ORELSE_RES, OpExp1_RES, ORELSE_SPAN : (Lex.pos * Lex.pos), OpExp1_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun OpExp_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (ANDALSO_RES, ANDALSO_SPAN, strm') = matchANDALSO(strm)
                  val (OpExp1_RES, OpExp1_SPAN, strm') = OpExp1_NT(strm')
                  val FULL_SPAN = (#1(ANDALSO_SPAN), #2(OpExp1_SPAN))
                  in
                    (UserCode.OpExp_PROD_1_SUBRULE_1_PROD_2_ACT (ANDALSO_RES, OpExp1_RES, ANDALSO_SPAN : (Lex.pos * Lex.pos), OpExp1_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.ANDALSO, _, strm') =>
                    OpExp_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.ORELSE, _, strm') => OpExp_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      fun OpExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ORELSE, _, strm') => true
              | (Tok.ANDALSO, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(OpExp_PROD_1_SUBRULE_1_PRED, OpExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(OpExp1_SPAN), #2(SR_SPAN))
      in
        (UserCode.OpExp_PROD_1_ACT (SR_RES, OpExp1_RES, SR_SPAN : (Lex.pos * Lex.pos), OpExp1_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and OpExp1_NT (strm) = let
      val (OpExp2_RES, OpExp2_SPAN, strm') = OpExp2_NT(strm)
      fun OpExp1_PROD_1_SUBRULE_1_NT (strm) = let
            fun OpExp1_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (EQEQ_RES, EQEQ_SPAN, strm') = matchEQEQ(strm)
                  val (OpExp2_RES, OpExp2_SPAN, strm') = OpExp2_NT(strm')
                  val FULL_SPAN = (#1(EQEQ_SPAN), #2(OpExp2_SPAN))
                  in
                    (UserCode.OpExp1_PROD_1_SUBRULE_1_PROD_1_ACT (EQEQ_RES, OpExp2_RES, EQEQ_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun OpExp1_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (NEQ_RES, NEQ_SPAN, strm') = matchNEQ(strm)
                  val (OpExp2_RES, OpExp2_SPAN, strm') = OpExp2_NT(strm')
                  val FULL_SPAN = (#1(NEQ_SPAN), #2(OpExp2_SPAN))
                  in
                    (UserCode.OpExp1_PROD_1_SUBRULE_1_PROD_2_ACT (NEQ_RES, OpExp2_RES, NEQ_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun OpExp1_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val (LT_RES, LT_SPAN, strm') = matchLT(strm)
                  val (OpExp2_RES, OpExp2_SPAN, strm') = OpExp2_NT(strm')
                  val FULL_SPAN = (#1(LT_SPAN), #2(OpExp2_SPAN))
                  in
                    (UserCode.OpExp1_PROD_1_SUBRULE_1_PROD_3_ACT (LT_RES, OpExp2_RES, LT_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun OpExp1_PROD_1_SUBRULE_1_PROD_4 (strm) = let
                  val (LTEQ_RES, LTEQ_SPAN, strm') = matchLTEQ(strm)
                  val (OpExp2_RES, OpExp2_SPAN, strm') = OpExp2_NT(strm')
                  val FULL_SPAN = (#1(LTEQ_SPAN), #2(OpExp2_SPAN))
                  in
                    (UserCode.OpExp1_PROD_1_SUBRULE_1_PROD_4_ACT (LTEQ_RES, OpExp2_RES, LTEQ_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.LTEQ, _, strm') => OpExp1_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.NEQ, _, strm') => OpExp1_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.EQEQ, _, strm') => OpExp1_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.LT, _, strm') => OpExp1_PROD_1_SUBRULE_1_PROD_3(strm)
                | _ => fail()
              (* end case *))
            end
      fun OpExp1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.EQEQ, _, strm') => true
              | (Tok.NEQ, _, strm') => true
              | (Tok.LTEQ, _, strm') => true
              | (Tok.LT, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(OpExp1_PROD_1_SUBRULE_1_PRED, OpExp1_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(OpExp2_SPAN), #2(SR_SPAN))
      in
        (UserCode.OpExp1_PROD_1_ACT (SR_RES, OpExp2_RES, SR_SPAN : (Lex.pos * Lex.pos), OpExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and OpExp2_NT (strm) = let
      val (OpExp3_RES, OpExp3_SPAN, strm') = OpExp3_NT(strm)
      fun OpExp2_PROD_1_SUBRULE_1_NT (strm) = let
            val (CONS_RES, CONS_SPAN, strm') = matchCONS(strm)
            val (OpExp3_RES, OpExp3_SPAN, strm') = OpExp3_NT(strm')
            val FULL_SPAN = (#1(CONS_SPAN), #2(OpExp3_SPAN))
            in
              (UserCode.OpExp2_PROD_1_SUBRULE_1_PROD_1_ACT (CONS_RES, OpExp3_RES, CONS_SPAN : (Lex.pos * Lex.pos), OpExp3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun OpExp2_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.CONS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(OpExp2_PROD_1_SUBRULE_1_PRED, OpExp2_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(OpExp3_SPAN), #2(SR_SPAN))
      in
        (UserCode.OpExp2_PROD_1_ACT (SR_RES, OpExp3_RES, SR_SPAN : (Lex.pos * Lex.pos), OpExp3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and OpExp3_NT (strm) = let
      val (OpExp4_RES, OpExp4_SPAN, strm') = OpExp4_NT(strm)
      fun OpExp3_PROD_1_SUBRULE_1_NT (strm) = let
            fun OpExp3_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (CONCAT_RES, CONCAT_SPAN, strm') = matchCONCAT(strm)
                  val (OpExp4_RES, OpExp4_SPAN, strm') = OpExp4_NT(strm')
                  val FULL_SPAN = (#1(CONCAT_SPAN), #2(OpExp4_SPAN))
                  in
                    (UserCode.OpExp3_PROD_1_SUBRULE_1_PROD_1_ACT (CONCAT_RES, OpExp4_RES, CONCAT_SPAN : (Lex.pos * Lex.pos), OpExp4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun OpExp3_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
                  val (OpExp4_RES, OpExp4_SPAN, strm') = OpExp4_NT(strm')
                  val FULL_SPAN = (#1(PLUS_SPAN), #2(OpExp4_SPAN))
                  in
                    (UserCode.OpExp3_PROD_1_SUBRULE_1_PROD_2_ACT (PLUS_RES, OpExp4_RES, PLUS_SPAN : (Lex.pos * Lex.pos), OpExp4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun OpExp3_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
                  val (OpExp4_RES, OpExp4_SPAN, strm') = OpExp4_NT(strm')
                  val FULL_SPAN = (#1(MINUS_SPAN), #2(OpExp4_SPAN))
                  in
                    (UserCode.OpExp3_PROD_1_SUBRULE_1_PROD_3_ACT (MINUS_RES, OpExp4_RES, MINUS_SPAN : (Lex.pos * Lex.pos), OpExp4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.MINUS, _, strm') => OpExp3_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.CONCAT, _, strm') =>
                    OpExp3_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.PLUS, _, strm') => OpExp3_PROD_1_SUBRULE_1_PROD_2(strm)
                | _ => fail()
              (* end case *))
            end
      fun OpExp3_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.CONCAT, _, strm') => true
              | (Tok.PLUS, _, strm') => true
              | (Tok.MINUS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(OpExp3_PROD_1_SUBRULE_1_PRED, OpExp3_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(OpExp4_SPAN), #2(SR_SPAN))
      in
        (UserCode.OpExp3_PROD_1_ACT (SR_RES, OpExp4_RES, SR_SPAN : (Lex.pos * Lex.pos), OpExp4_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and OpExp4_NT (strm) = let
      val (OpExp5_RES, OpExp5_SPAN, strm') = OpExp5_NT(strm)
      fun OpExp4_PROD_1_SUBRULE_1_NT (strm) = let
            fun OpExp4_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
                  val (OpExp5_RES, OpExp5_SPAN, strm') = OpExp5_NT(strm')
                  val FULL_SPAN = (#1(TIMES_SPAN), #2(OpExp5_SPAN))
                  in
                    (UserCode.OpExp4_PROD_1_SUBRULE_1_PROD_1_ACT (TIMES_RES, OpExp5_RES, TIMES_SPAN : (Lex.pos * Lex.pos), OpExp5_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun OpExp4_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (DIV_RES, DIV_SPAN, strm') = matchDIV(strm)
                  val (OpExp5_RES, OpExp5_SPAN, strm') = OpExp5_NT(strm')
                  val FULL_SPAN = (#1(DIV_SPAN), #2(OpExp5_SPAN))
                  in
                    (UserCode.OpExp4_PROD_1_SUBRULE_1_PROD_2_ACT (DIV_RES, OpExp5_RES, DIV_SPAN : (Lex.pos * Lex.pos), OpExp5_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun OpExp4_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val (MOD_RES, MOD_SPAN, strm') = matchMOD(strm)
                  val (OpExp5_RES, OpExp5_SPAN, strm') = OpExp5_NT(strm')
                  val FULL_SPAN = (#1(MOD_SPAN), #2(OpExp5_SPAN))
                  in
                    (UserCode.OpExp4_PROD_1_SUBRULE_1_PROD_3_ACT (MOD_RES, OpExp5_RES, MOD_SPAN : (Lex.pos * Lex.pos), OpExp5_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.MOD, _, strm') => OpExp4_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.TIMES, _, strm') => OpExp4_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.DIV, _, strm') => OpExp4_PROD_1_SUBRULE_1_PROD_2(strm)
                | _ => fail()
              (* end case *))
            end
      fun OpExp4_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | (Tok.DIV, _, strm') => true
              | (Tok.MOD, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(OpExp4_PROD_1_SUBRULE_1_PRED, OpExp4_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(OpExp5_SPAN), #2(SR_SPAN))
      in
        (UserCode.OpExp4_PROD_1_ACT (SR_RES, OpExp5_RES, SR_SPAN : (Lex.pos * Lex.pos), OpExp5_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and OpExp5_NT (strm) = let
      fun OpExp5_PROD_1 (strm) = let
            val (DEREF_RES, DEREF_SPAN, strm') = matchDEREF(strm)
            val (ApplyExp_RES, ApplyExp_SPAN, strm') = ApplyExp_NT(strm')
            val FULL_SPAN = (#1(DEREF_SPAN), #2(ApplyExp_SPAN))
            in
              (UserCode.OpExp5_PROD_1_ACT (DEREF_RES, ApplyExp_RES, DEREF_SPAN : (Lex.pos * Lex.pos), ApplyExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun OpExp5_PROD_2 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (ApplyExp_RES, ApplyExp_SPAN, strm') = ApplyExp_NT(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(ApplyExp_SPAN))
            in
              (UserCode.OpExp5_PROD_2_ACT (ApplyExp_RES, MINUS_RES, ApplyExp_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun OpExp5_PROD_3 (strm) = let
            val (ApplyExp_RES, ApplyExp_SPAN, strm') = ApplyExp_NT(strm)
            val FULL_SPAN = (#1(ApplyExp_SPAN), #2(ApplyExp_SPAN))
            in
              ((ApplyExp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_case, _, strm') => OpExp5_PROD_3(strm)
          | (Tok.LP, _, strm') => OpExp5_PROD_3(strm)
          | (Tok.LCB, _, strm') => OpExp5_PROD_3(strm)
          | (Tok.UID(_), _, strm') => OpExp5_PROD_3(strm)
          | (Tok.LID(_), _, strm') => OpExp5_PROD_3(strm)
          | (Tok.NUMBER(_), _, strm') => OpExp5_PROD_3(strm)
          | (Tok.STRING(_), _, strm') => OpExp5_PROD_3(strm)
          | (Tok.DEREF, _, strm') => OpExp5_PROD_1(strm)
          | (Tok.MINUS, _, strm') => OpExp5_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and ApplyExp_NT (strm) = let
      val (AtomicExp_RES, AtomicExp_SPAN, strm') = AtomicExp_NT(strm)
      fun ApplyExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (AtomicExp_RES, AtomicExp_SPAN, strm') = AtomicExp_NT(strm)
            val FULL_SPAN = (#1(AtomicExp_SPAN), #2(AtomicExp_SPAN))
            in
              ((AtomicExp_RES), FULL_SPAN, strm')
            end
      fun ApplyExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_case, _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.LCB, _, strm') => true
              | (Tok.UID(_), _, strm') => true
              | (Tok.LID(_), _, strm') => true
              | (Tok.NUMBER(_), _, strm') => true
              | (Tok.STRING(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(ApplyExp_PROD_1_SUBRULE_1_PRED, ApplyExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AtomicExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.ApplyExp_PROD_1_ACT (SR_RES, AtomicExp_RES, SR_SPAN : (Lex.pos * Lex.pos), AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AtomicExp_NT (strm) = let
      fun AtomicExp_PROD_1 (strm) = let
            val (UID_RES, UID_SPAN, strm') = matchUID(strm)
            val FULL_SPAN = (#1(UID_SPAN), #2(UID_SPAN))
            in
              (UserCode.AtomicExp_PROD_1_ACT (UID_RES, UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_2 (strm) = let
            val (LID_RES, LID_SPAN, strm') = matchLID(strm)
            val FULL_SPAN = (#1(LID_SPAN), #2(LID_SPAN))
            in
              (UserCode.AtomicExp_PROD_2_ACT (LID_RES, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_3 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.AtomicExp_PROD_3_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_4 (strm) = let
            val (NUMBER_RES, NUMBER_SPAN, strm') = matchNUMBER(strm)
            val FULL_SPAN = (#1(NUMBER_SPAN), #2(NUMBER_SPAN))
            in
              (UserCode.AtomicExp_PROD_4_ACT (NUMBER_RES, NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_5 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            fun AtomicExp_PROD_5_SUBRULE_1_NT (strm) = let
                  val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm)
                  fun AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                        
                        val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                        val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
                        val FULL_SPAN = (#1(COMMA_SPAN), #2(Exp_SPAN))
                        in
                          (UserCode.AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (LP_RES, Exp_RES, COMMA_RES, LP_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = 
                        (case (lex(strm))
                         of (Tok.COMMA, _, strm') => true
                          | _ => false
                        (* end case *))
                  val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_PRED, AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
                  val FULL_SPAN = (#1(Exp_SPAN), #2(SR_SPAN))
                  in
                    (UserCode.AtomicExp_PROD_5_SUBRULE_1_PROD_1_ACT (LP_RES, SR_RES, Exp_RES, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun AtomicExp_PROD_5_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_case, _, strm') => true
                    | (Tok.KW_if, _, strm') => true
                    | (Tok.LP, _, strm') => true
                    | (Tok.LCB, _, strm') => true
                    | (Tok.MINUS, _, strm') => true
                    | (Tok.DEREF, _, strm') => true
                    | (Tok.UID(_), _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | (Tok.NUMBER(_), _, strm') => true
                    | (Tok.STRING(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(AtomicExp_PROD_5_SUBRULE_1_PRED, AtomicExp_PROD_5_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomicExp_PROD_5_ACT (LP_RES, SR_RES, RP_RES, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_6 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (Scope_RES, Scope_SPAN, strm') = (Scope_NT (UserCode.ARGS_63 (LCB_RES)))(strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.AtomicExp_PROD_6_ACT (RCB_RES, LCB_RES, Scope_RES, RCB_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_7 (strm) = let
            val (KW_case_RES, KW_case_SPAN, strm') = matchKW_case(strm)
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val (KW_of_RES, KW_of_SPAN, strm') = matchKW_of(strm')
            fun AtomicExp_PROD_7_SUBRULE_1_NT (strm) = let
                  val (MatchCase_RES, MatchCase_SPAN, strm') = MatchCase_NT(strm)
                  val FULL_SPAN = (#1(MatchCase_SPAN), #2(MatchCase_SPAN))
                  in
                    ((MatchCase_RES), FULL_SPAN, strm')
                  end
            fun AtomicExp_PROD_7_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LCB, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.posclos(AtomicExp_PROD_7_SUBRULE_1_PRED, AtomicExp_PROD_7_SUBRULE_1_NT, strm')
            val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
            val FULL_SPAN = (#1(KW_case_SPAN), #2(KW_end_SPAN))
            in
              (UserCode.AtomicExp_PROD_7_ACT (SR_RES, KW_end_RES, KW_of_RES, Exp_RES, KW_case_RES, SR_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_of_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), KW_case_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_case, _, strm') => AtomicExp_PROD_7(strm)
          | (Tok.LP, _, strm') => AtomicExp_PROD_5(strm)
          | (Tok.STRING(_), _, strm') => AtomicExp_PROD_3(strm)
          | (Tok.UID(_), _, strm') => AtomicExp_PROD_1(strm)
          | (Tok.LID(_), _, strm') => AtomicExp_PROD_2(strm)
          | (Tok.NUMBER(_), _, strm') => AtomicExp_PROD_4(strm)
          | (Tok.LCB, _, strm') => AtomicExp_PROD_6(strm)
          | _ => fail()
        (* end case *))
      end
and MatchCase_NT (strm) = let
      val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
      val (Pat_RES, Pat_SPAN, strm') = Pat_NT(strm')
      val (DARROW_RES, DARROW_SPAN, strm') = matchDARROW(strm')
      val (Scope_RES, Scope_SPAN, strm') = (Scope_NT (UserCode.ARGS_70 (Pat_RES, LCB_RES, DARROW_RES)))(strm')
      val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
      val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
      in
        (UserCode.MatchCase_PROD_1_ACT (Pat_RES, RCB_RES, LCB_RES, DARROW_RES, Scope_RES, Pat_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Scope_NT (env_RES) (strm) = let
      fun Scope_PROD_1 (strm) = let
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm)
            fun Scope_PROD_1_SUBRULE_1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val (Scope_RES, Scope_SPAN, strm') = (Scope_NT (UserCode.ARGS_66 (SEMI_RES, env_RES, Exp_RES)))(strm')
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(Scope_SPAN))
                  in
                    ((Scope_RES), FULL_SPAN, strm')
                  end
            fun Scope_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(Scope_PROD_1_SUBRULE_1_PRED, Scope_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Exp_SPAN), #2(SR_SPAN))
            in
              (UserCode.Scope_PROD_1_ACT (SR_RES, env_RES, Exp_RES, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Scope_PROD_2 (strm) = let
            val (ValBind_RES, ValBind_SPAN, strm') = ValBind_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (Scope_RES, Scope_SPAN, strm') = (Scope_NT (UserCode.ARGS_68 (ValBind_RES, SEMI_RES, env_RES)))(strm')
            val FULL_SPAN = (#1(ValBind_SPAN), #2(Scope_SPAN))
            in
              (UserCode.Scope_PROD_2_ACT (ValBind_RES, SEMI_RES, env_RES, Scope_RES, ValBind_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') => Scope_PROD_2(strm)
          | (Tok.KW_let, _, strm') => Scope_PROD_2(strm)
          | (Tok.KW_case, _, strm') => Scope_PROD_1(strm)
          | (Tok.KW_if, _, strm') => Scope_PROD_1(strm)
          | (Tok.LP, _, strm') => Scope_PROD_1(strm)
          | (Tok.LCB, _, strm') => Scope_PROD_1(strm)
          | (Tok.MINUS, _, strm') => Scope_PROD_1(strm)
          | (Tok.DEREF, _, strm') => Scope_PROD_1(strm)
          | (Tok.UID(_), _, strm') => Scope_PROD_1(strm)
          | (Tok.LID(_), _, strm') => Scope_PROD_1(strm)
          | (Tok.NUMBER(_), _, strm') => Scope_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => Scope_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Type_NT (strm) = let
      val (AtomicTypeList_RES, AtomicTypeList_SPAN, strm') = AtomicTypeList_NT(strm)
      fun Type_PROD_1_SUBRULE_1_NT (strm) = let
            val (ARROW_RES, ARROW_SPAN, strm') = matchARROW(strm)
            val (AtomicTypeList_RES, AtomicTypeList_SPAN, strm') = AtomicTypeList_NT(strm')
            val FULL_SPAN = (#1(ARROW_SPAN), #2(AtomicTypeList_SPAN))
            in
              (UserCode.Type_PROD_1_SUBRULE_1_PROD_1_ACT (AtomicTypeList_RES, ARROW_RES, AtomicTypeList_SPAN : (Lex.pos * Lex.pos), ARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ARROW, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Type_PROD_1_SUBRULE_1_PRED, Type_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AtomicTypeList_SPAN), #2(SR_SPAN))
      in
        (UserCode.Type_PROD_1_ACT (SR_RES, AtomicTypeList_RES, SR_SPAN : (Lex.pos * Lex.pos), AtomicTypeList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AtomicTypeList_NT (strm) = let
      val (AtomicType_RES, AtomicType_SPAN, strm') = AtomicType_NT(strm)
      fun AtomicTypeList_PROD_1_SUBRULE_1_NT (strm) = let
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
            val (AtomicType_RES, AtomicType_SPAN, strm') = AtomicType_NT(strm')
            val FULL_SPAN = (#1(TIMES_SPAN), #2(AtomicType_SPAN))
            in
              (UserCode.AtomicTypeList_PROD_1_SUBRULE_1_PROD_1_ACT (TIMES_RES, AtomicType_RES, TIMES_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicTypeList_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomicTypeList_PROD_1_SUBRULE_1_PRED, AtomicTypeList_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AtomicType_SPAN), #2(SR_SPAN))
      in
        (UserCode.AtomicTypeList_PROD_1_ACT (SR_RES, AtomicType_RES, SR_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
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
              (UserCode.TopDcl_PROD_2_ACT (ValBind_RES, ValBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') => TopDcl_PROD_2(strm)
          | (Tok.KW_let, _, strm') => TopDcl_PROD_2(strm)
          | (Tok.KW_data, _, strm') => TopDcl_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Program_NT (env_RES) (strm) = let
      fun Program_PROD_1 (strm) = let
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm)
            fun Program_PROD_1_SUBRULE_1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val (Program_RES, Program_SPAN, strm') = (Program_NT (UserCode.ARGS_5 (SEMI_RES, env_RES, Exp_RES)))(strm')
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(Program_SPAN))
                  in
                    ((Program_RES), FULL_SPAN, strm')
                  end
            fun Program_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(Program_PROD_1_SUBRULE_1_PRED, Program_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Exp_SPAN), #2(SR_SPAN))
            in
              (UserCode.Program_PROD_1_ACT (SR_RES, env_RES, Exp_RES, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Program_PROD_2 (strm) = let
            val (TopDcl_RES, TopDcl_SPAN, strm') = TopDcl_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (Program_RES, Program_SPAN, strm') = (Program_NT (UserCode.ARGS_7 (TopDcl_RES, SEMI_RES, env_RES)))(strm')
            val FULL_SPAN = (#1(TopDcl_SPAN), #2(Program_SPAN))
            in
              (UserCode.Program_PROD_2_ACT (TopDcl_RES, SEMI_RES, env_RES, Program_RES, TopDcl_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Program_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_data, _, strm') => Program_PROD_2(strm)
          | (Tok.KW_fun, _, strm') => Program_PROD_2(strm)
          | (Tok.KW_let, _, strm') => Program_PROD_2(strm)
          | (Tok.KW_case, _, strm') => Program_PROD_1(strm)
          | (Tok.KW_if, _, strm') => Program_PROD_1(strm)
          | (Tok.LP, _, strm') => Program_PROD_1(strm)
          | (Tok.LCB, _, strm') => Program_PROD_1(strm)
          | (Tok.MINUS, _, strm') => Program_PROD_1(strm)
          | (Tok.DEREF, _, strm') => Program_PROD_1(strm)
          | (Tok.UID(_), _, strm') => Program_PROD_1(strm)
          | (Tok.LID(_), _, strm') => Program_PROD_1(strm)
          | (Tok.NUMBER(_), _, strm') => Program_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => Program_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun PreProg_NT (strm) = let
      val (Program_RES, Program_SPAN, strm') = (Program_NT (UserCode.ARGS_3 ()))(strm)
      val FULL_SPAN = (#1(Program_SPAN), #2(Program_SPAN))
      in
        ((Program_RES), FULL_SPAN, strm')
      end
in
  (PreProg_NT)
end
val PreProg_NT =  fn s => unwrap (Err.launch (eh, lexFn, PreProg_NT , true) s)

in (PreProg_NT) end
  in
fun parse lexFn  s = let val (PreProg_NT) = mk lexFn in PreProg_NT s end

  end

end
