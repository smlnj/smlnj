(* Copyright (c) 1998 by Lucent Technologies *)

(* sizeof.sml

 * rules for bit-fields:
   
    - cannot be more than sizeof an int (word)
    - can be zero (only if there is no id) : means fill to word 
    - need not have id
    - can straddle boundary of words (very implementation
      dependent); behavior specified by S.bitFieldAlignment.

 *)

structure Sizeof : SIZEOF =
struct 

  structure Tid = Tid
  structure B = Bindings
  structure S = Sizes
  structure TU = TypeUtil
  structure TypeCheckControl = Config.TypeCheckControl

  structure Map = BinaryMapFn (struct
				   type ord_key = Tid.uid
				   val compare = Tid.compare
			       end)

  val warningsRef = ref true
  fun warningsOn () = warningsRef := true
  fun warningsOff () = warningsRef := false
  fun localWarning s = if !warningsRef then TextIO.print s else ()

  (* ref used for memoization of sizeof values *)
  val tidSizeAlignMapRef =
      ref (Map.empty : {tabOpt: {memberOpt: Ast.member option, bitOffset:int} list option,
			bits: int, align: int} Map.map)

  fun reset() = 
      tidSizeAlignMapRef := 
        (Map.empty : {tabOpt:{memberOpt: Ast.member option, bitOffset:int} list option,
		      bits: int, align: int} Map.map)

  fun padToBoundary{bits, boundary} = 
    let val q = Int.mod (bits,boundary)
    in if q = 0 then bits else bits + (boundary - q) end

  (* used as a bogus return value *)
  val defaultIntLayout = 
      let val {bits, align} = #int Sizes.defaultSizes
       in {tabOpt=NONE:({memberOpt:Ast.member option, bitOffset:int} list) option,
	   bits=bits, align=align}
      end

  fun fieldSizeStruct (sizesErrWarnBug as {sizes, err, warn, bug})
                      tidtab (ctype, memberOpt, SOME li) =
	let val errors =
		case TU.getCoreType tidtab ctype
	          of Ast.Numeric(_, _, _, Ast.FLOAT, _) =>
		       err "Can't mix bitfield and float."
		   | Ast.Numeric(_, _, _, Ast.DOUBLE, _) =>
		       err "Can't mix bitfield and double."
		   | Ast.Numeric(_, _, _, Ast.LONGDOUBLE, _) =>
		       err "Can't mix bitfield and longdouble."
		   | Ast.Numeric(_, _, _, Ast.CHAR, _) => 
		       if TypeCheckControl.ISO_bitfield_restrictions
			   then err "Can't mix bitfield and char in ISO/ANSI C." 
		                (* (ISO spec, section 6.5.2.1, p60) *)
		       else ()
		   | Ast.Numeric(_, _, _, Ast.SHORT, _) => 
		       if TypeCheckControl.ISO_bitfield_restrictions
			   then err "Can't mix bitfield and short in ISO/ANSI C."
		                (* (ISO spec, section 6.5.2.1, p60) *)
		       else ()
		   | Ast.Numeric(_, _, _, Ast.LONG, _) => 
		       if TypeCheckControl.ISO_bitfield_restrictions
			   then err "Can't mix bitfield and long in ISO/ANSI C."
		                (* (ISO spec, section 6.5.2.1, p60) *)
		       else ()
		   | Ast.Numeric(_, _, _, Ast.LONGLONG, _) => 
		       if TypeCheckControl.ISO_bitfield_restrictions
			   then err "Can't mix bitfield and long long in ISO/ANSI C."
		                (* (ISO spec, section 6.5.2.1, p60) *)
		       else ()
		   | Ast.Numeric(_, _, _, Ast.INT, _) => ()
		   | (Ast.EnumRef _) =>
			 if TypeCheckControl.allow_enum_bitfields then ()
			 else err "Enum not permitted in bitfield."
		   | _ => err "Bitfield must be numeric (char, short, int)"
	    val i = LargeInt.toInt li
	    val {bits, align,...} = process sizesErrWarnBug tidtab ctype
	 in if i > bits then err "Width of field exceeds its type" else ();
	    {memberOpt=memberOpt, bitfield=SOME i, size=bits, align=align}
	end
    | fieldSizeStruct sizesErrWarnBug tidtab (ctype, memberOpt, NONE) = 
	let val {bits, align,...} = process sizesErrWarnBug tidtab ctype
	 in {memberOpt=memberOpt, bitfield=NONE, size=bits, align=align}
	end

  and fieldSizeUnion sizesErrWarnBug tidtab (ctype, member) = 
	let val {bits, align,...} = process sizesErrWarnBug tidtab ctype
	 in {bits=bits, align=align}
	end


	(* The basic idea is to process bit-fields in order from first to last,
	   inserting padding as necessary, accumulating alignment constraints,
	   and recording for each field the bit offset from the start of the struct.
	   The alignment constraints of the underlying types of bit fields are propagated
           to the alignment constraints of the entire structure (with some exceptions;
	   see below).

	   Although the standard only mandates bitfields with underlying type
	   int (signed or unsigned), most compilers allow for bitfields
	   of type char, short or long (possible signed or unsigned) as well.
	   The difference is reflected in the alignment constraints.

	   The basic algorithm is as follows.  There are two main variables
  	   a) alignmentSoFar: alignment constraint so far encountered
           b) nextBit: next bit to be allocated (starts with 0)
	               NB: corresponds to how many bits so far layed out in this struct


           To process a bitfield with type t and size b bits, where layout(t) = {size, align}

           if b>0 then
		1. if b > size then indicate error.
		2. alignmentSoFar := max(alignmentSoFar, align)
		3. if (nextBit + b) div size <> nextBit div size
	           /* i.e. adding this field would cross a "size" boundary */
		       pad nextBit to next "size" boundary
                4. struct[field] := nextBit
		5. nextBit += b
	   else /* b == 0 */
                6. alignmentSoFar := max(alignmentSoFar, align)
		7. pad nextBit to next "size" boundary

	   ASSUMPTIONS: alignments are powers of 2

	   COMPLICATIONS:
	   A. Only allow int (int, unsigned, signed) bitfields.
	      This is controlled by the flag TypeCheckControl.ISO_bitfield_restrictions
              (default = false).
              If set to true, then an error is raised 
              for bitfields with types other than int, unsigned, signed.
	      
           B. Do unnamed bitfields contribute to alignment constraints?
	      Most compilers say no (except lcc).
	      This is controlled by the sizes.sml flag ignoreUnnamedBitFieldAlignment (default true).
	      If set, then the alignment of unnamed bitfields is ignored (i.e. only
	      their size counts).
                 e.g.
                      struct X {int :8; char x; char y;}  sizeof(struct X) = 3 (true) or 4 (false)
	      
           C. Are non bitfields packed with bitfields?
              C1: Only pack bit fields (sizes.sml flag: onlyPackBitFields)
	          if flag is true, then start the current bitfield on a size boundary
                  unless previous field was a bitfield.
 		  e.g. struct X {char x; int z: 5;}   sizeof(struct X) = 4 (false) or 8 (true)

	      C2: In theory there is a complementary variation involving non-bitfields after
 	          bitfields, but it is not clear what this might mean (although
		  that's never stopped someone putting it into a c compiler), and 
		  it isn't implemented in ckit.

       ----------------------------------------------------------------
	 Old notes on unnamed length zero bit fields:

	 Haberson and Steele p 138 says
	 "Specifying a (bit field) length of 0 for an unnamed bit field has a
	  special meaning - it indicates that the following component should
	  begin on the next boundary appropriate to its type.  ("Appropriate"
	  is not specified further; in ISO C, it is the next int-size unit.)"

	 We implement the following (which seems to be what SGI cc and gcc do):
	 Specifying a (bit field) length of 0 for an unnamed bit field indicates
	 that the following component should be aligned according to the
	 alignment constraints of the unnamed bit field.  (Of course if the
	 next field has its own alignment constriants, e.g. is double, then
	 the next fields alignment constraints must also be satisfied.)

	 Note: this interpretation differs from ISO (and also K&R p 150) if
	 char and short bit fields are involved e.g.

	       struct s { char a : 4;
			  short  : 0;
			  char b : 2;
			};
	 *)

  and computeFieldStruct {sizes: Sizes.sizes, err, warn, bug}
        {nextBit, alignmentSoFar, lastFieldWasBitField,
	 field={memberOpt, bitfield=SOME bits, size, align}} =
	if bits > 0 then
	  let
	    val nextBit =  (* pad out if last field not bitfield and onlyPackBitFields *)
	      if #onlyPackBitFields sizes andalso not lastFieldWasBitField
		then padToBoundary{bits=nextBit, boundary=size}
	      else nextBit
	    val alignmentSoFar =    (* accumulate alignment constraints *)
	      (case memberOpt of
		 NONE => if #ignoreUnnamedBitFieldAlignment sizes then alignmentSoFar
			 else Int.max (alignmentSoFar,align)
	       | SOME _ => Int.max (alignmentSoFar,align))
	    val fieldStartBit = (* pad out if we cross a "size" boundary *)
	      if (nextBit + bits) div size = nextBit div size
		then nextBit
	      else padToBoundary{bits=nextBit, boundary=size}
	  in (* NB: checking for error case of (bits > size) is done in fieldSizeStruct *)
	    {field={memberOpt=memberOpt, bitOffset=nextBit},
	     nextBit=nextBit + bits,
	     alignmentSoFar=alignmentSoFar,
	     lastFieldWasBitField=true}
	  end
	else (* bits = 0 *)
	  let 
	    val alignmentSoFar = if #ignoreUnnamedBitFieldAlignment sizes
				   then alignmentSoFar
				 else Int.max (alignmentSoFar,align)
	    val nextBit = padToBoundary{bits=nextBit, boundary=size}
	    val _ = (case memberOpt of
		       NONE => ()
		     | _ => err "Named bit-field has zero width")
	  in
	    {field={memberOpt=memberOpt, bitOffset=nextBit},
	     nextBit=nextBit,
	     alignmentSoFar=alignmentSoFar,
	     lastFieldWasBitField=true}
	  end
    | computeFieldStruct {sizes, err, warn, bug}
        {nextBit, alignmentSoFar, lastFieldWasBitField,
	 field={memberOpt, bitfield=NONE, size, align}} =
      let 
	val thisBit = padToBoundary{bits=nextBit, boundary=align}
	val alignmentSoFar = Int.max(alignmentSoFar, align)
      in
	{field={memberOpt=memberOpt, bitOffset=thisBit},
	 nextBit=thisBit + size,
	 alignmentSoFar=alignmentSoFar,
	 lastFieldWasBitField=false}
      end

  and computeFieldListStruct (sizesErrWarnBug as {sizes, err, warn, bug})
                             tidtab fieldList =
      let val l = List.map (fieldSizeStruct sizesErrWarnBug tidtab) fieldList
	  fun foldfn (field, {tab, nextBit, alignmentSoFar, lastFieldWasBitField}) =
	      let val {field, nextBit, alignmentSoFar, lastFieldWasBitField} =
		      computeFieldStruct sizesErrWarnBug {nextBit=nextBit,
							  alignmentSoFar=alignmentSoFar,
							  field=field,
							  lastFieldWasBitField=lastFieldWasBitField}
	      in
		  {tab=field :: tab,
		   nextBit=nextBit,
		   alignmentSoFar=alignmentSoFar,
		   lastFieldWasBitField=lastFieldWasBitField}
	      end
	  val {tab, nextBit, alignmentSoFar, lastFieldWasBitField} =
	      List.foldl foldfn {tab=nil,
				 nextBit=0,
				 alignmentSoFar=(#align(#min_struct sizes)),
				 lastFieldWasBitField=false} l
      in
	{tab=List.rev tab, nextBit=nextBit, align=alignmentSoFar}
      end
    
      
  and computeFieldListUnion (sizesErrWarnBug as {sizes, err, warn, bug})
                            tidtab fieldList =
      let 
	val l = List.map (fieldSizeUnion sizesErrWarnBug tidtab)
	                 fieldList
	fun foldfn ({bits=fieldBits,align=fieldAlign}, {size, align}) =
	    {size=Int.max(size, fieldBits), align=Int.max(align, fieldAlign)}
	       (* again, assume alignments are powers of 2 *)
      in
	  foldr foldfn {size=0, align=(#align(#min_union sizes))} l
      end
      
		   
  and processTid (sizesErrWarnBug as {sizes, err, warn, bug})
                 (tidtab: Tables.tidtab) tid =
    case Map.find (!tidSizeAlignMapRef, tid)
      of SOME result => result
       | NONE => 
	   let val result =
		   case Tidtab.find (tidtab,tid)
		     of SOME({ntype=SOME(B.Struct (_,fields)),...}) =>
			 let val {tab, nextBit, align, ...} =
			         computeFieldListStruct sizesErrWarnBug
				    tidtab fields
			  in {tabOpt=SOME tab, bits=padToBoundary{bits=nextBit, boundary=align},
			      align=align}
			 end
		      | SOME({ntype=SOME(B.Union (_,fields)),...}) =>
			 let val {size, align} =
			         computeFieldListUnion sizesErrWarnBug
				    tidtab fields
			  in {tabOpt=NONE,
			      bits=padToBoundary{bits=size, boundary=align},
			      align=align}
			 end
		      | SOME({ntype=SOME(B.Typedef (_,ty)),...}) => process sizesErrWarnBug tidtab ty
		      | SOME({ntype=SOME(B.Enum _),...}) =>
			 let val {bits, align} = #int sizes
			  in {tabOpt=NONE, bits=bits, align=align}
			 end
		      | SOME{ntype=NONE,...} =>
			 (err
			   "sizeof applied to a partial type";
                          defaultIntLayout)
		      | NONE  =>
			 (bug
			   "sizeof: missing type id in type-id map.";
                          defaultIntLayout)
	   in 
	       tidSizeAlignMapRef :=  Map.insert (!tidSizeAlignMapRef, tid, result);
	       result
	   end
	
  and process (sizesErrWarnBug as {sizes, err, warn, bug}) tidtab ty =
      case ty
	of Ast.TypeRef tid => processTid sizesErrWarnBug tidtab tid
	 | (Ast.StructRef tid | Ast.UnionRef tid) =>
	     processTid sizesErrWarnBug tidtab tid
	 | Ast.EnumRef _ => 
	     let val {bits,align} = #int sizes
	     in {tabOpt=NONE,bits=bits,align=align} end
	 | Ast.Qual (_,ty) => process sizesErrWarnBug tidtab ty
	 | Ast.Array (SOME(n, _) ,ty) =>
	     let val {tabOpt, bits=sz, align} = process sizesErrWarnBug tidtab ty
	     in {tabOpt=NONE, bits = (LargeInt.toInt n) * sz, align=align} end
	 | Ast.Array(NONE,ty) => 
	     ( err "taking sizeof array whose size is unspecified: assuming unit size.\n"
	     ; let val {bits,align,...} = process sizesErrWarnBug tidtab ty
	        in {tabOpt = NONE, bits = bits, align = align}
	       end
	     )
	 | Ast.Pointer _ =>
	     let val {bits,align} = #pointer sizes
	     in {tabOpt=NONE,bits=bits,align=align} end
	 | Ast.Numeric (_, _, _,ik, _) =>
	     let val {char,short,int,long,longlong,float,double,longdouble,...}
		     = sizes
		 val {bits,align} = case ik
				      of Ast.CHAR   => char
				       | Ast.SHORT  => short
				       | Ast.INT    => int
				       | Ast.LONG   => long
				       | Ast.LONGLONG => longlong
				       | Ast.FLOAT  => float
				       | Ast.DOUBLE => double
				       | Ast.LONGDOUBLE => longdouble
	     in {tabOpt=NONE,bits=bits,align=align} end
	 | Ast.Function _ => 
	     let val {bits,align} = #pointer sizes
	     in {tabOpt=NONE,bits=bits,align=align} end
	 | Ast.Error => 
		 let val {bits,align} = #int sizes
		  in {tabOpt=NONE,bits=bits,align=align}
		 end
	 |  _ => let val {bits,align} = #int sizes
		  in err "invalid type to be sized: assuming int size.\n";
		     {tabOpt=NONE,bits=bits,align=align}
		 end

  fun toBytes bits =
      if (bits mod 8) = 0 then bits div 8
      else ( localWarning "Warning: toBytes is rounding your bits."
	   ; bits div 8
	   )
	  
  fun byteSizeOf sizesErrWarnBug tidtab ty =
      let val {bits,align,...} = process sizesErrWarnBug tidtab ty
       in {bytes=toBytes bits,
	   byteAlignment=toBytes align}
      end


  fun bitSizeOf sizesErrWarnBug tidtab ty =
      let val {bits,align,...} = process sizesErrWarnBug tidtab ty
       in {bits=bits,
	   bitAlignment=align}
      end


  fun fieldOffsets sizesErrWarnBug tidtab ty =
      #tabOpt(process sizesErrWarnBug tidtab ty)
       
  fun equalMember({uid=uid1,...}: Ast.member, {uid=uid2,...}: Ast.member) =
      Pid.equal(uid1,uid2)

  fun getField {sizes, err, warn, bug} (member,[]) =
        (err "field not found";
	 {memberOpt = NONE, bitOffset=0})
    | getField sizesErrWarnBug (member,{memberOpt=NONE,...}::fields) =
	getField sizesErrWarnBug (member,fields)
    | getField sizesErrWarnBug (member,(field as {memberOpt=SOME member',bitOffset})::fields) =
        if equalMember (member,member') then field
	else getField sizesErrWarnBug (member,fields)

end (* structure Sizeof *)
