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

    fun bindAllVar (cxt, []) = cxt
      | bindAllVar (cxt, fst::rest) = C.bindVar (cxt, fst, BT.VarId.new fst )

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
                        val cxt' = bindAllVar (cxt', params)
                        val cxt' = C.bindTyCon (cxt', tyid, BT.TycId.new tyid)
                        val castParams = List.map ( fn (x) => BT.TyVar.new x ) params 

                        fun chkCons (cxt, [], cons') = (BT.DclData (BT.TycId.new tyid, castParams, List.rev cons'), cxt)
                        | chkCons (cxt, con::cons, cons') = let
                            val (con', cxt) = chkCon (cxt, con)
                            in
                                chkCons (cxt, cons, con'::cons')
                            end
                                in
                                  chkCons (cxt', cons, [])
                                end)
                          (*End of Case*)
          and chkCon _ = raise Fail "TODO"
          and chkExp _ = raise Fail "TODO"
          in
            chkProg (C.new errS, prog)
          end (* analyze *)

  end (* Binding *)
