(* binding.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Binding analysis for ML Lite.
 *)

structure Binding : sig

    (* check the bindings in a ML Lite parse-tree and return a binding-tree
     * representation that makes the connection between binding and use
     * occurrences of identifiers explicit.
     *)
    val analyze : Error.err_stream * ParseTree.program -> BindTree.program

  end = struct

    structure PT = ParseTree
    structure BT = BindTree
    structure C = Context

    (* dummy binding-trees that we can use when an error prevents us from constructing
     * an actual tree.
     *)
    val bogusTy = BT.TyTuple[]
    val bogusExp = BT.ExpTuple[]
    val bogusPat = BT.PatWild

    (* The following two helper functions are used to process the mark nodes
     * in the parse tree.
     *
     * `chkWithMark wrap chk (cxt, {span, tree})` applies the `chk` function
     * to `tree` using a context that has been updated with the `span`.  The
     * resulting bind-tree form is then paired with span and wrapped by the
     * bind-tree constructor `wrap`.
     *)
    fun chkWithMark wrap chk (cxt, {span, tree}) =
          wrap {span = span, tree = chk (C.setSpan(cxt, span), tree)}

    (* `chkWithMark'` is similar to `chkWithMark`, except that it handles
     * `chk` functions that return an extended context.
     *)
    fun chkWithMark' wrap chk (cxt, {span, tree}) = let
          val (tree', cxt') = chk (C.setSpan(cxt, span), tree)
          in
            (wrap {span = span, tree = tree'}, cxt')
          end
    
    fun samewrap a b = Atom.same (a, b)

    (*Checks if a list of atoms has any repeats*)  
    fun repeats [] = NONE
      | repeats (fst :: rest) = (case (List.find (samewrap fst) rest) of
                                      NONE => (repeats rest)
                                    | SOME a => SOME a)
    
    fun analyze (errS, prog) = let
          (* report an unbound-identifier error *)
          fun unbound (cxt, kind, id) =
                Error.errorAt(errS, C.spanOf cxt, [
                    "unbound ", kind, " `", Atom.toString id, "`"
                  ])
          (* report a duplicate identifier error; the second argument specifies
           * the kind of identifier as a string.
           *)
          fun duplicate (cxt, kind, x) = Error.errorAt (errS, C.spanOf cxt, [
                  "duplicate ", kind, " `", Atom.toString x, "` "
                ])
          (* analyze a program *)
          fun chkProg (cxt, PT.ProgMark m) = chkWithMark BT.ProgMark chkProg (cxt, m)
            | chkProg (cxt, PT.Prog(dcls, exp)) = let
                (* process each of the top-level declarations while accumulating their
                 * bindings in the context.
                 *)
                fun chkDcls (cxt, [], dcls') = BT.Prog(List.rev dcls', chkExp(cxt, exp))
                  | chkDcls (cxt, dcl::dcls, dcls') = let
                      val (dcl', cxt) = chkDcl (cxt, dcl)
                      in
                        chkDcls (cxt, dcls, dcl'::dcls')
                      end
                in
                  chkDcls (cxt, dcls, [])
                end
          and chkDcl (cxt, PT.DclMark m) = (chkWithMark' BT.DclMark chkDcl (cxt,m))
            | chkDcl (cxt, PT.DclData (tyid, params, cons)) = 
                    (case (repeats params) of
                      SOME a => let
                        val _ = duplicate (cxt, "DclData", a)
                                in
                                  (BT.DclData (BT.TycId. new tyid, [], []), cxt)
                                end
                    | NONE => let
                        val cxt' = C.new (C.errStrmOf cxt)
                        val cxt' = C.setTVEnv (cxt', C.tvEnvOf cxt)
                        val tyid' = BT.TycId.new tyid
                        val cxt' = C.bindTyCon (cxt', tyid, tyid')
                        val castParams = List.map ( fn (x) => BT.TyVar.new x ) params 

                        fun chkCons (cxt, [], cons') = (BT.DclData (tyid', castParams, List.rev cons'), cxt)
                        | chkCons (cxt, con::cons, cons') = let
                            val (con', cxt) = chkCon (cxt, con)
                            in
                                chkCons (cxt, cons, con'::cons')
                            end
                                in
                                  chkCons (cxt', cons, [])
                                end)
                          (*End of Case*)
            | chkDcl (cxt, PT.DclVal bind) = let
              val (bind', cxt') = chkVdcl (cxt, bind)
              in
                (BT.DclVal ( bind' ), cxt')
              end
              
          and chkCon ( cxt, PT.ConMark m ) = (chkWithMark' BT.ConMark chkCon (cxt,m))
            | chkCon ( cxt, PT.Con (id, opt) ) = (case opt of
                      NONE => (case C.findCon (cxt, id) of
                          NONE => let
                              val id' = BT.ConId.new id
                            in
                              (BT.Con ( id', NONE ), C.mergeConEnv (cxt , AtomMap.insert ( AtomMap.empty, id, id' ) ) )
                            end
                        | SOME b => let
                            val _ = duplicate ( cxt, "con", id )
                              in
                            (BT.Con (BT.ConId.new id, NONE), cxt)
                              end) 
                        (*End of case*)
                      | SOME a => (case C.findCon (cxt, id) of
                          NONE => let
                              val id' = BT.ConId.new id
                            in
                              (BT.Con ( id', SOME (chkTy (cxt, a) )), C.mergeConEnv(cxt , AtomMap.insert (AtomMap.empty, id, id' )))
                            end
                        | SOME b => let
                            val _ = duplicate ( cxt, "Con", id )
                              in
                            (BT.Con (BT.ConId.new id, SOME (chkTy (cxt, a) )), cxt)
                              end))
                          (*End of case*)
                          (*End of case*)
          and chkTy (cxt, PT.TyMark m ) = (chkWithMark BT.TyMark chkTy (cxt, m))
            | chkTy (cxt, PT.TyVar t) = 
                (case (C.findTyVar (cxt, t)) of
                    NONE => let
                      val _ = unbound (cxt, "TyVar", t)
                        in
                      BT.TyVar (BT.TyVar.new t)
                        end
                  | SOME a => BT.TyVar a)
            | chkTy (cxt, PT.TyFun (t1, t2)) = BT.TyFun ( chkTy (cxt, t1), chkTy (cxt, t2))
            | chkTy (cxt, PT.TyCon (con, tyList)) = 
                (case (C.findTyCon (cxt, con)) of
                    NONE => let
                      val _ = unbound ( cxt, "TyCon", con )
                        in
                      BT.TyCon (BT.TycId.new con, [])
                        end
                  | SOME a => BT.TyCon ( a, List.map ( fn x => chkTy (cxt, x) ) tyList))
            | chkTy (cxt, PT.TyTuple ( lst )) = BT.TyTuple ( List.map ( fn x => chkTy (cxt, x) ) lst )

          and chkVdcl (cxt, PT.BindMark m) = chkWithMark' BT.BindMark chkVdcl (cxt, m)
            | chkVdcl (cxt, PT.BindExp exp) = ( BT.BindExp ( chkExp (cxt, exp)), cxt )
            | chkVdcl (cxt, PT.BindVal ( pat, exp )) = let
              val cxt' = C.new (C.errStrmOf cxt)
              val cxt' = C.mergeConEnv (cxt', C.conEnvOf cxt)
              val (pat', cxt') = chkPat (cxt', pat)
              val exp' = chkExp (cxt, exp)
                in
              (BT.BindVal ( pat', exp' ), C.mergeVarEnv ( cxt, C.varEnvOf cxt' ))
                end

            | chkVdcl (cxt, PT.BindFun ( id, pats, exp )) = let
              val cxt' = C.new (C.errStrmOf cxt)
              val cxt' = C.mergeConEnv (cxt', C.conEnvOf cxt)

              fun chkPats (cxt, []) = ([], cxt)
                | chkPats (cxt, (fst::rest)) = let
                  val (pat', cxt') = chkPat (cxt, fst)
                  val (pats, cxt') = chkPats (cxt', rest)
                    in
                  (pat' :: pats, cxt')
                    end
              
              val (pats', cxt') = chkPats (cxt', pats)
              val id'  = BT.VarId.new id
              val cxt' = C.bindVar ( cxt', id, id' )
              val exp' = chkExp (cxt', exp)
                in
              (BT.BindFun ( id', List.rev pats', exp'), C.bindVar (cxt, id, id') )
                end
              
              
          and chkExp (cxt, PT.ExpMark m) = chkWithMark BT.ExpMark chkExp (cxt, m)
            | chkExp (cxt, PT.ExpIf (exp1, exp2, exp3))
                = BT.ExpIf (chkExp (cxt, exp1), chkExp (cxt, exp2), chkExp (cxt, exp3))
            | chkExp (cxt, PT.ExpOrElse (exp1, exp2))
                = BT.ExpOrElse ( chkExp (cxt, exp1), chkExp (cxt,exp2) )
            | chkExp (cxt, PT.ExpAndAlso (exp1, exp2))
                = BT.ExpAndAlso ( chkExp (cxt, exp1), chkExp (cxt,exp2) )
            | chkExp (cxt, PT.ExpBin (exp1, id, exp2))
                = BT.ExpBin ( chkExp (cxt, exp1), BT.VarId.new id, chkExp (cxt,exp2) )
            | chkExp (cxt, PT.ExpApp (exp1, exp2))
                = BT.ExpApp ( chkExp (cxt, exp1), chkExp (cxt,exp2) )
            | chkExp (cxt, PT.ExpUn (id, exp1))
                = BT.ExpUn ( BT.VarId.new id, chkExp (cxt,exp1) )
            | chkExp (cxt, PT.ExpListCons (exp1, exp2))
                = BT.ExpListCons ( chkExp (cxt, exp1), chkExp (cxt,exp2) )
            | chkExp (cxt, PT.ExpTuple (exps))
                = BT.ExpTuple ( List.map (fn x => chkExp ( cxt, x )) exps )
            | chkExp (cxt, PT.ExpCase (exp, cases))
                = BT.ExpCase ( chkExp (cxt, exp), List.map (fn x => chkRule ( cxt, x )) cases )
            | chkExp (cxt, PT.ExpVar id) = (case (C.findVar (cxt, id)) of
                                                   NONE => let
                                                     val _ = unbound ( cxt, "ExpVar", id )
                                                        in
                                                      BT.ExpVar ( BT.VarId.new id )
                                                        end
                                                 | SOME a => BT.ExpVar ( a ))
            | chkExp (cxt, PT.ExpCon id) = (case (C.findCon (cxt, id)) of
                                                   NONE => let
                                                     val _ = unbound ( cxt, "ExpCon", id )
                                                        in
                                                      BT.ExpCon ( BT.ConId.new id )
                                                        end
                                                 | SOME a => BT.ExpCon ( a ))
            | chkExp (cxt, PT.ExpInt num) = BT.ExpInt num
            | chkExp (cxt, PT.ExpStr str) = BT.ExpStr str
            | chkExp (cxt, PT.ExpScope scope) = BT.ExpScope (chkScope (cxt, scope))

          and chkPat (cxt, PT.PatMark m) = chkWithMark' BT.PatMark chkPat (cxt, m)
            | chkPat (cxt, PT.PatVar id) = (case (C.findVar (cxt, id) ) of
                                              NONE => let 
                                                val id' = (BT.VarId.new id)
                                                val cxt' = C.bindVar (cxt, id,
                                                id')
                                                  in
                                                (BT.PatVar id', cxt')
                                                  end
                                            | SOME a => let
                                              val _ = duplicate (cxt, "PatVar", id)
                                                in
                                              (BT.PatVar ( BT.VarId.new id ), cxt)
                                                end)
            | chkPat (cxt, PT.PatCon ( id, opt )) 
              = (case opt of
                  NONE => (case C.findCon (cxt, id) of
                                NONE => let
                                  val _ = unbound (cxt, "PatCon", id)
                                    in
                                  (BT.PatCon (BT.ConId.new id, NONE), cxt)
                                    end
                              | SOME a => (BT.PatCon (a, NONE), cxt))
                | SOME b => (case C.findCon (cxt, id) of
                                NONE => let
                                  val _ = unbound (cxt, "PatCon", id)
                                    in
                                  (BT.PatCon (BT.ConId.new id, NONE), cxt)
                                    end
                              | SOME a => let
                                val (pat', cxt') = chkPat (cxt, b)
                                  in
                                (BT.PatCon (a, SOME pat'), cxt')
                                  end))
            | chkPat (cxt, PT.PatListCons (pat1, pat2)) = let
                val (pat1', cxt') = chkPat (cxt, pat1)
                val (pat2', cxt'') = chkPat (cxt, pat2)
                val cxt' = C.mergeVarEnv (cxt', C.varEnvOf cxt'')
                  in
                (BT.PatListCons (pat1', pat2'), cxt')
                  end
            | chkPat (cxt, PT.PatTuple (lst)) = let
                fun chkPats (cxt, []) = ([], cxt)
                  | chkPats (cxt, fst::rest) = let
                    val (fst', cxt') = chkPat (cxt, fst)
                      in
                    (fst' :: #1 (chkPats (cxt', rest)), cxt')
                      end
                val (lst, cxt') = chkPats (cxt, lst)
                  in
                (BT.PatTuple ( lst ), cxt')
                  end

            | chkPat (cxt, PT.PatWild) = (BT.PatWild, cxt)
          and chkRule (cxt, PT.RuleMark m) =  chkWithMark BT.RuleMark chkRule (cxt, m)
            | chkRule (cxt, PT.RuleCase (pat, scope) ) = let
                val cxt' = C.new (C.errStrmOf cxt)
                val cxt' = C.mergeConEnv ( cxt', C.conEnvOf cxt )
                val (pat', cxt') = chkPat (cxt', pat)
                val scope' = chkScope (C.mergeVarEnv (cxt, C.varEnvOf cxt'), scope)
                  in
                (BT.RuleCase (pat', scope'))
                  end
          and chkScope (cxt, (lst, exp)) = let
            fun chkDcls ( cxt, [] ) = []
              | chkDcls ( cxt, fst::rest ) = let 
                  val (fst', cxt') = chkVdcl (cxt, fst) 
                    in
                  fst' :: (chkDcls (cxt', rest))
                    end
              in
            (chkDcls (cxt, lst), chkExp (cxt, exp))
              end
          in
            chkProg (C.new errS, prog)
          end (* analyze *)

  end (* Binding *)
