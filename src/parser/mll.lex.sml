structure MLLLex  = struct

    datatype yystart_state = 
BLK_COM | STRING | LN_COM | INITIAL
    local

    structure UserDeclarations = 
      struct



    structure T = MLLTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* the depth int ref is used for keeping track of comment depth *)
    val depth = ref 0

  (* starting position of comments/string; used for error reporting *)
    val startPos : Error.pos ref = ref 0

  (* list of string fragments to concatenate *)
    val buf : string list ref = ref []

  (* add a string to the buffer *)
    fun addStr s = (buf := s :: !buf)

  (* make a string from buf *)
    fun mkString () = (T.STRING(String.concat(List.rev(!buf))) before buf := [])

  (* keyword lookup table *)
    local
      val find =
          let val tbl = AtomTable.mkTable (17, Fail "keywords")
              fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
          in
              app ins [
                ("case",        T.KW_case),
                ("data",        T.KW_data),
                ("else",        T.KW_else),
                ("end",         T.KW_end),
                ("fun",         T.KW_fun),
                ("if",          T.KW_if),
                ("let",         T.KW_let),
                ("of",          T.KW_of),
                ("then",        T.KW_then)
              ];
              AtomTable.find tbl
          end
    in
  (* return either a keyword token or a LID token *)
    fun idToken id = let
          val ida = Atom.atom id
          in
            case find ida
             of NONE => T.LID ida
              | SOME kw => kw
          end
    end (* local *)

      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yyarg as  lexErr)(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case ULexBuffer.getu strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LP)
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RP)
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LB)
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RB)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LCB)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RCB)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  T.ASSIGN)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  T.ORELSE)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  T.ANDALSO)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  T.EQEQ)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;  T.NEQ)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LTEQ)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LT)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;  T.CONS)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  T.CONCAT)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;  T.PLUS)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;  T.MINUS)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  T.TIMES)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;  T.DIV)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  T.MOD)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  T.DEREF)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  T.EQ)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  T.COMMA)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  T.SEMI)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  T.BAR)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  T.ARROW)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;  T.DARROW)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;  T.WILD)
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  idToken yytext
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.UID(Atom.atom yytext)
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.NUMBER(valOf (IntInf.fromString yytext))
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;  skip ())
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN BLK_COM; startPos := yypos; depth := 1; skip())
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN LN_COM; skip())
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN STRING; startPos := yypos; continue())
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr((yypos, yypos), [
                                "bad character `", String.toString yytext, "'"
                              ]);
                            skip()
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; continue()
      end
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
       lexErr((yypos, yypos), [
                                "illegal escape sequence '\\000' in string"
                              ]);
                            continue())
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr(valOf(String.fromString yytext)); continue()
      end
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; mkString())
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr((yypos, yypos), [
                                "bad escape character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            continue()
      end
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
       lexErr((!startPos, yypos), [
                                "unclosed string at end of line"
                              ]);
                            YYBEGIN INITIAL; mkString())
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr((yypos, yypos), [
                                "bad character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            continue()
      end
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
       depth := !depth + 1;
                            skip())
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
       depth := !depth - 1;
                            if (!depth = 0) then YYBEGIN INITIAL else ();
                            skip ())
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;  skip ())
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; skip())
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx7C
              then yyQ56(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ57(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ57(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ57(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ57(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ57(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ57(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ57(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ57(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction29(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction29(strm, yyNO_MATCH)
                      else yyQ58(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ58(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction29(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ58(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ58(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction29(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction29(strm, yyNO_MATCH)
                      else yyQ58(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ58(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction29(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ58(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ58(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ60(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < 0wx3E
              then if inp = 0wx3D
                  then yyQ59(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ61(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyAction35(strm, yyNO_MATCH)
            else if inp < 0wx3B
              then if inp = 0wx3A
                  then yyQ62(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = 0wx3D
              then yyQ63(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ64(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ64(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ64(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ64(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2B
              then yyAction18(strm, yyNO_MATCH)
            else if inp < 0wx2B
              then if inp = 0wx2A
                  then yyQ65(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp = 0wx2F
              then yyQ66(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ67(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx26
              then yyQ68(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ69(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of LN_COM => ( T.EOF)
                    | BLK_COM =>
                        ( lexErr((!startPos, yypos), [
                                "unclosed comment at end of file"
                              ]);
                            T.EOF)
                    | STRING =>
                        ( lexErr((!startPos, yypos), [
                                "unclosed string at end of file"
                              ]);
                            T.EOF)
                    | INITIAL => ( T.EOF)
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ42(strm', lastMatch)
            else if inp < 0wx30
              then if inp = 0wx27
                  then yyQ29(strm', lastMatch)
                else if inp < 0wx27
                  then if inp = 0wx21
                      then yyQ31(strm', lastMatch)
                    else if inp < 0wx21
                      then if inp = 0wxE
                          then yyQ29(strm', lastMatch)
                        else if inp < 0wxE
                          then if inp <= 0wx8
                              then yyQ29(strm', lastMatch)
                              else yyQ30(strm', lastMatch)
                        else if inp = 0wx20
                          then yyQ30(strm', lastMatch)
                          else yyQ29(strm', lastMatch)
                    else if inp = 0wx25
                      then yyQ33(strm', lastMatch)
                    else if inp < 0wx25
                      then if inp = 0wx22
                          then yyQ32(strm', lastMatch)
                          else yyQ29(strm', lastMatch)
                      else yyQ34(strm', lastMatch)
                else if inp = 0wx2C
                  then yyQ39(strm', lastMatch)
                else if inp < 0wx2C
                  then if inp = 0wx2A
                      then yyQ37(strm', lastMatch)
                    else if inp < 0wx2A
                      then if inp = 0wx28
                          then yyQ35(strm', lastMatch)
                          else yyQ36(strm', lastMatch)
                      else yyQ38(strm', lastMatch)
                else if inp = 0wx2E
                  then yyQ29(strm', lastMatch)
                else if inp = 0wx2D
                  then yyQ40(strm', lastMatch)
                  else yyQ41(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ49(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx3D
                  then yyQ46(strm', lastMatch)
                else if inp < 0wx3D
                  then if inp = 0wx3B
                      then yyQ44(strm', lastMatch)
                    else if inp < 0wx3B
                      then if inp = 0wx3A
                          then yyQ43(strm', lastMatch)
                          else yyQ42(strm', lastMatch)
                      else yyQ45(strm', lastMatch)
                else if inp = 0wx5B
                  then yyQ48(strm', lastMatch)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyQ29(strm', lastMatch)
                      else yyQ47(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ53(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx60
                  then yyQ29(strm', lastMatch)
                else if inp < 0wx60
                  then if inp = 0wx5E
                      then yyQ50(strm', lastMatch)
                      else yyQ51(strm', lastMatch)
                  else yyQ52(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ55(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ54(strm', lastMatch)
              else yyQ29(strm', lastMatch)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ28(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of LN_COM => ( T.EOF)
                    | BLK_COM =>
                        ( lexErr((!startPos, yypos), [
                                "unclosed comment at end of file"
                              ]);
                            T.EOF)
                    | STRING =>
                        ( lexErr((!startPos, yypos), [
                                "unclosed string at end of file"
                              ]);
                            T.EOF)
                    | INITIAL => ( T.EOF)
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyQ25(strm', lastMatch)
            else if inp < 0wxB
              then if inp = 0wxA
                  then yyQ26(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = 0wxD
              then yyQ27(strm', lastMatch)
              else yyQ25(strm', lastMatch)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ20(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ20(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ19(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction40(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ19(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx31
              then yyQ20(strm', lastMatch)
            else if inp < 0wx31
              then if inp = 0wx30
                  then yyQ22(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ20(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx31
              then yyQ19(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < 0wx31
              then if inp = 0wx30
                  then yyQ21(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyAction40(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ19(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5D
              then yyQ15(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < 0wx5D
              then if inp = 0wx30
                  then yyQ17(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ16(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                      else yyQ15(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp = 0wx3A
                  then yyQ15(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < 0wx3A
                  then yyQ18(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp = 0wx5C
                  then yyQ16(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = 0wx72
              then yyQ16(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx6E
                  then yyQ16(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ16(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyQ15(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx20
                  then yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < 0wx20
                  then yyAction36(strm, yyNO_MATCH)
                else if inp = 0wx22
                  then yyAction36(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp = 0wx5D
              then yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyAction36(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx20
                  then yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < 0wx20
                  then yyAction36(strm, yyNO_MATCH)
                else if inp = 0wx22
                  then yyAction36(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp = 0wx5D
              then yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyAction36(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ23(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ24(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of LN_COM => ( T.EOF)
                    | BLK_COM =>
                        ( lexErr((!startPos, yypos), [
                                "unclosed comment at end of file"
                              ]);
                            T.EOF)
                    | STRING =>
                        ( lexErr((!startPos, yypos), [
                                "unclosed string at end of file"
                              ]);
                            T.EOF)
                    | INITIAL => ( T.EOF)
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyQ12(strm', lastMatch)
            else if inp < 0wx20
              then if inp = 0wxB
                  then yyQ9(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ10(strm', lastMatch)
                      else yyQ9(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ11(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ14(strm', lastMatch)
            else if inp < 0wx5C
              then if inp = 0wx22
                  then yyQ13(strm', lastMatch)
                  else yyQ12(strm', lastMatch)
            else if inp <= 0wx7E
              then yyQ12(strm', lastMatch)
              else yyQ9(strm', lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ7(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyQ8(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of LN_COM => ( T.EOF)
                    | BLK_COM =>
                        ( lexErr((!startPos, yypos), [
                                "unclosed comment at end of file"
                              ]);
                            T.EOF)
                    | STRING =>
                        ( lexErr((!startPos, yypos), [
                                "unclosed string at end of file"
                              ]);
                            T.EOF)
                    | INITIAL => ( T.EOF)
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2B
              then yyQ4(strm', lastMatch)
            else if inp < 0wx2B
              then if inp = 0wx2A
                  then yyQ5(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = 0wx2F
              then yyQ6(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of BLK_COM => yyQ0(!(yystrm), yyNO_MATCH)
    | STRING => yyQ1(!(yystrm), yyNO_MATCH)
    | LN_COM => yyQ2(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ3(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()-1), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(yyarg as  lexErr)(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
yyarg(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
yyarg(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end

