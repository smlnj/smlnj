(* pp-ast.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PPAST : sig

    val output : TextIO.outstream * AST.module list -> unit

  end = struct

    structure PP = TextIOPP

    val indent0 = PP.Abs 0
    val indent2 = PP.Abs 2
    val indent4 = PP.Abs 4

    fun ppModule ppStrm (AST.Module{isPrim, id, decls}) = let
          fun sp () = PP.space ppStrm 1
	  fun brk () = PP.break ppStrm {nsp=1, offset=0}
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
        (* pretty print a named type *)
	  fun ppNamedTy ty = (case ty
		 of AST.BaseTy tyId => string(AST.TypeId.nameOf tyId)
		  | AST.ImportTy(modId, tyId) => (
		      string(AST.ModuleId.nameOf modId);
		      string ".";
		      string(AST.TypeId.nameOf tyId))
		  | AST.LocalTy(AST.TyDcl{id, ...}) => string(AST.TypeId.nameOf id)
		(* end case *))
	(* pretty print a type expression *)
	  fun ppTyExp (AST.Typ(ty, tyc)) = (
		PP.openHBox ppStrm;
		  ppNamedTy ty;
		  case tyc
		   of AST.NoTyc => ()
		    | AST.OptTyc => string "?"
		    | AST.SeqTyc => string "*"
		    | AST.SharedTyc => string "!"
		  (* end case *);
		PP.closeBox ppStrm)
	  fun ppDecl (AST.TyDcl{id, def, ...}) = let
		fun ppFields fields = let
		      fun ppField ({label, ty}, isFirst) = (
			    if isFirst then () else (string ","; sp());
			    ppTyExp ty;
			    case label
			     of SOME id => (sp(); string id)
			      | NONE => ()
			    (* end case *);
			    false)
		      in
			PP.openHBox ppStrm;
			  string "(";
			  ignore (List.foldl ppField true fields);
			  string ")";
			PP.closeBox ppStrm
		      end
handle ex => raise ex
		fun ppCons nAttrs (AST.Constr{id, fields, ...}, isFirst) = (
		      brk ();
		      PP.openHBox ppStrm;
			if isFirst then string "=" else string "|";
			sp();
		        string(AST.ConId.nameOf id);
			if (List.null fields)
			  then ()
			  else ppFields (List.drop (fields, nAttrs));
		      PP.closeBox ppStrm;
		      false)
handle ex => raise ex
		in
		  nl ();
		  string (AST.TypeId.nameOf id);
		  case !def
		   of AST.EnumTy cs => (
			PP.openHOVBox ppStrm indent2;
			  ignore (List.foldl (ppCons 0) true cs);
			PP.closeBox ppStrm)
		    | AST.SumTy{attribs, cons} => let
			val nAttribs = length attribs
			in
			  PP.openHOVBox ppStrm indent2;
			    ignore (List.foldl (ppCons nAttribs) true cons);
			  PP.closeBox ppStrm;
			  if List.null attribs
			    then ()
			    else (
			      PP.openVBox ppStrm indent4;
				nl();
				PP.openHBox ppStrm;
				  string "attributes"; sp(); ppFields attribs;
				PP.closeBox ppStrm;
			      PP.closeBox ppStrm)
			end
		    | AST.ProdTy{fields} => (
			PP.openHBox ppStrm;
			  sp(); string "="; sp();
			  ppFields fields;
			PP.closeBox ppStrm)
		    | AST.PrimTy => ()
		  (* end case *)
		end
handle ex => raise ex
	  in
	    PP.openVBox ppStrm indent2;
	      PP.openHBox ppStrm;
		if isPrim then (string "primitive"; sp()) else ();
		string "module"; sp();
		string (AST.ModuleId.nameOf id); sp(); string "{";
	      PP.closeBox ppStrm;
	      List.app ppDecl (!decls);
	    PP.closeBox ppStrm;
	    nl();
	    string "}";
	    nl()
	  end

    fun output (outS, modules) = let
          val ppStrm = PP.openOut {dst = outS, wid = 120}
	  in
	    PP.openVBox ppStrm indent0;
	      List.app (ppModule ppStrm) modules;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
	  end

  end


