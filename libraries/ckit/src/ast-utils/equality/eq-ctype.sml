(* Copyright (c) 1998 by Lucent Technologies *)

structure EqCType (*: EQCTYPE*) = struct

  structure Tid = Tid
  structure Pid = Pid
  structure B = Bindings
  structure CT = CType
  open CT

  exception eqFail

  fun warning s = (print "Warning: EqCType: "; print s; print "\n")

  fun myFold eq acc ([],[]) = acc
    | myFold eq acc (f1::fs1,f2::fs2) =
      myFold eq (eq acc (f1,f2)) (fs1,fs2)
    | myFold eq acc _ = raise eqFail

  fun eqList eq = myFold (fn bool => fn fs => bool andalso eq fs) true

  fun getCtypeBindings tidtabs maps ctPair =
      case ctPair
	of (Void,Void) => ()
         | (Ellipses,Ellipses) => ()
         | (Qual (q1,ct1),Qual (q2,ct2)) =>
	      getCtypeBindings tidtabs maps (ct1,ct2)
         | (Array (li1,ct1),Array (li2,ct2)) => 
	       getCtypeBindings tidtabs maps (ct1,ct2)
         | (Pointer ct1,Pointer ct2) =>
	       getCtypeBindings tidtabs maps (ct1,ct2)
         | (Function (ct1,cts1), Function (ct2,cts2)) => 
	       getCtypesBindings tidtabs maps (ct1::cts1,ct2::cts2)
         | (EnumRef tid1,EnumRef tid2) => getTidBindings tidtabs maps (tid1,tid2)
         | (StructRef tid1,StructRef tid2) => getTidBindings tidtabs maps (tid1,tid2)
         | (UnionRef tid1,UnionRef tid2) => getTidBindings tidtabs maps (tid1,tid2)
         | (TypeRef tid1,TypeRef tid2) => getTidBindings tidtabs maps (tid1,tid2)
	 | _ => ()

  and getCtypesBindings tidtabs maps ctPairs = 
      (map (getCtypeBindings tidtabs maps) (ListPair.zip ctPairs); ())

  and getTidBindings (tidtab1: Tables.tidtab,tidtab2: Tables.tidtab)
                     (maps as (tidmap,pidmap)) (tid1,tid2) =
      case Tidtab.find (tidmap,tid1)
	of SOME tid2' => ()
         | NONE => case (Tidtab.find (tidtab1,tid1),Tidtab.find (tidtab2,tid2))
	             of (SOME {ntype=SOME nct1,...},SOME {ntype=SOME nct2,...}) =>
			 ( Tidtab.insert (tidmap,tid1,tid2)
			 ; getNamedCtypeBindings (tidtab1,tidtab2) maps (nct1,nct2)
                         )
		      | _ => Tidtab.insert (tidmap,tid1,tid2)

  and getNamedCtypeBindings tidtabs (maps as (tidmap,pidmap)) nctPair =
      case nctPair
	of (B.Struct (tid1,fields1),B.Struct (tid2,fields2)) =>
	      let 
		  fun getField () ((ct1,memOpt1:Ast.member option,_)
				  ,(ct2,memOpt2:Ast.member option,_)) =
		      ( getCtypeBindings tidtabs maps (ct1,ct2)
		      ; case (memOpt1,memOpt2)
			  of (SOME {uid=pid1,...},SOME {uid=pid2,...}) =>
			      Pidtab.insert (pidmap,pid1,pid2)
			   | _ => ()
		      )
	      in
		  ( Tidtab.insert (tidmap,tid1,tid2)
		  ; myFold getField () (fields1,fields2)
		  )
	      end
	 | (B.Union (tid1,fields1), B.Union (tid2,fields2)) =>
	      let 
		  fun getField () ((ct1,{uid=pid1,...}:Ast.member),(ct2,{uid=pid2,...}:Ast.member)) =
		      ( Pidtab.insert (pidmap,pid1,pid2)
		      ; getCtypeBindings tidtabs maps (ct1,ct2)
		      )
	      in
		  ( Tidtab.insert (tidmap,tid1,tid2)
		  ; myFold getField () (fields1,fields2)
		  )
	      end
          | (B.Enum (tid1,fields1),B.Enum (tid2,fields2)) =>
		let fun getField () (({uid=pid1,...}:Ast.member,_)
				    ,({uid=pid2,...}:Ast.member,_)) =
		    Pidtab.insert (pidmap,pid1,pid2)
		in
		  ( Tidtab.insert (tidmap,tid1,tid2)
		  ; myFold getField () (fields1,fields2)
		  )
		end
	  | (B.Typedef (tid1,ct1),B.Typedef (tid2,ct2)) =>
		( Tidtab.insert (tidmap,tid1,tid2)
		; getCtypeBindings tidtabs (tidmap,pidmap)(ct1,ct2)
		)
	  | _ => ()

  fun eqTid tidmap (tid1,tid2) =
      case Tidtab.find (tidmap,tid1)
	of NONE => ( warning ("tid ("^(Tid.toString tid1)^") not found, reverting to simple equality test")
		   ; Tid.equal (tid1,tid2)
		   )
         | SOME tid1' => Tid.equal (tid1',tid2)

  fun eqPid pidmap (pid1,pid2) =
      case Pidtab.find (pidmap,pid1)
	of NONE => ( warning ("pid ("^(Pid.toString pid1)^") not found, reverting to simple equality test")
		   ; Pid.equal (pid1,pid2)
		   )
         | SOME pid1' => Pid.equal (pid1',pid2)

  fun eqMem pidmap ({uid=pid1,...}:Ast.member ,{uid=pid2, ...}:Ast.member) = eqPid pidmap (pid1,pid2)

  fun eqMemOpt pidmap (NONE,NONE) = true
    | eqMemOpt pidmap (SOME mem1,SOME mem2) = eqMem pidmap (mem1,mem2)
    | eqMemOpt pidmap _ = false

  fun eqCtype tidmap ctPair =
      case ctPair
	of (Void,Void) => true
         | (Ellipses,Ellipses) => true
         | (Qual (q1,ct1),Qual (q2,ct2)) =>
	       if q1 = q2 then eqCtype tidmap (ct1,ct2)
	       else false
         | (Numeric quad1,Numeric quad2) => quad1 = quad2
         | (Array (li1,ct1),Array (li2,ct2)) => 
	       if li1 = li2 then eqCtype tidmap (ct1,ct2) else false
         | (Pointer ct1,Pointer ct2) => eqCtype tidmap (ct1,ct2)
         | (Function (ct1,cts1), Function (ct2,cts2)) => 
	       eqCtypes tidmap (ct1::cts1,ct2::cts2)
         | (EnumRef tid1,EnumRef tid2) => eqTid tidmap (tid1,tid2)
         | (StructRef tid1,StructRef tid2) => eqTid tidmap (tid1,tid2)
         | (UnionRef tid1,UnionRef tid2) => eqTid tidmap (tid1,tid2)
         | (TypeRef tid1,TypeRef tid2) => eqTid tidmap (tid1,tid2)
	 | _ => false

  and eqCtypes tidmap = eqList (eqCtype tidmap)
     
  and eqNamedCtype (pair as (tidmap,pidmap)) nctPair =
      case nctPair
	of (B.Struct (tid1,fields1),B.Struct (tid2,fields2)) =>
	      let 
		  fun eqField ((ct1,memOpt1,LIOpt1),(ct2,memOpt2,LIOpt2)) =
		      LIOpt1 = LIOpt2
		      andalso eqMemOpt pidmap (memOpt1,memOpt2)
		      andalso eqCtype tidmap (ct1,ct2)
		  val eqFields = eqList eqField
	      in eqTid tidmap (tid1,tid2) andalso eqFields (fields1,fields2)
	      end
	 | (B.Union (tid1,fields1),B.Union (tid2,fields2)) =>
	      let 
		  fun eqField ((ct1,mem1),(ct2,mem2)) =
		      eqMem pidmap (mem1,mem2) andalso eqCtype tidmap (ct1,ct2)
		  val eqFields = eqList eqField
	      in eqTid tidmap (tid1,tid2) andalso eqFields (fields1,fields2)
	      end
          | (B.Enum (tid1,fields1),B.Enum (tid2,fields2)) =>
		let fun eqField ((mem1,li1),(mem2,li2)) =
		      li1 = li2 andalso eqMem pidmap (mem1,mem2)
		    val eqFields = eqList eqField
		in
		    eqTid tidmap (tid1,tid2) andalso eqFields (fields1,fields2)
		end
	  | (B.Typedef (tid1,ct1),B.Typedef (tid2,ct2)) =>
		eqTid tidmap (tid1,tid2) andalso eqCtype tidmap (ct1,ct2)
	  | _ => false

end
