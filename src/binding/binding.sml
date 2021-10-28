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
          and chkDcl (cxt, PT.DclMark m) = (chkWithMark BT.DclMark (fn x => #1(chkDcl x)) (cxt,m), cxt)
            | chkDcl (cxt, PT.DclData (tyid, params, cons)) = 
                    (case (repeats params) of
                      SOME a => let
                        val _ = duplicate (cxt, "var", a)
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
              
          and chkCon ( cxt, PT.ConMark m ) = (chkWithMark BT.ConMark ( fn x => #1 (chkCon x)) (cxt, m), cxt)
            | chkCon ( cxt, PT.Con (id, opt) ) = (case opt of
                      NONE => (case C.findCon (cxt, id) of
                          NONE => let
                              val id' = BT.ConId.new id
                            in
                              (BT.Con ( id', NONE ), C.mergeConEnv (cxt , AtomMap.insert ( AtomMap.empty, id, id' ) ) )
                            end
                        | SOME a => raise Fail "TODO: dupe error" ) 
                        (*End of case*)
                      | SOME a => (case C.findCon (cxt, id) of
                          NONE => let
                              val id' = BT.ConId.new id
                            in
                              (BT.Con ( id', SOME (chkTy (cxt, a) )), C.mergeConEnv(cxt , AtomMap.insert (AtomMap.empty, id, id' )))
                            end 
                        | SOME A => raise Fail "TODO: dupe error")
                          (*End of case*))
                          (*End of case*)
          and chkTy (cxt, PT.TyMark m ) = (chkWithMark BT.TyMark chkTy (cxt, m))
            | chkTy (cxt, PT.TyVar t) = 
                (case (C.findTyVar (cxt, t)) of
                    NONE => raise Fail "TODO: unbound typevar"
                  | SOME a => BT.TyVar a)
            | chkTy (cxt, PT.TyFun (t1, t2)) = BT.TyFun ( chkTy (cxt, t1), chkTy (cxt, t2))
            | chkTy (cxt, PT.TyCon (con, tyList)) = 
                (case (C.findTyCon (cxt, con)) of
                    NONE => raise Fail "TODO: unboud tycon"
                  | SOME a => BT.TyCon ( a, List.map ( fn x => chkTy (cxt, x) ) tyList))
            | chkTy (cxt, PT.TyTuple ( lst )) = BT.TyTuple ( List.map ( fn x => chkTy (cxt, x) ) lst )

          and chkVdcl (cxt, PT.BindMark m) = (chkWithMark BT.BindMark ( fn x => #1 (chkVdcl x)) (cxt, m), cxt)
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
              val cxt' = C.mergeConEnv (cxt', C.conEnvOf cxt )
              
          and chkExp _ = raise Fail "TODO"
          and chkPat _ = raise Fail "TODO" 
          in
            chkProg (C.new errS, prog)
          end (* analyze *)

  end (* Binding *)
