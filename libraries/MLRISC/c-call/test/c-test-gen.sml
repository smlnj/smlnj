(* c-test-gen.sml
 * 
 * This module tests C calls for staged allocation.  We generate tests for a variety
 * of calls.  To verify the test, we generate a dummy C function that prints out its 
 * parameters in order.
 *
 * The example code below passes arguments x and y to the MLRISC-generated C call. The output
 * of main.c and sanity.c should be identical.

 /* glue.c */
 #define MAX_SZ 16

 int target (int x, float y)
 {
   printf("%d %f", x, y);
   return 23432;
 }

 void glueCode() 
 {
      void* arr0[4096];
      void** arr = arr0;
      /* initialize arguments */
      int x = 23423;
      memcpy(arr, &x, sizeof(int));
      arr += MAX_SZ;
      float y = 1024.013f;
      memcpy(arr, &y, sizeof(float));
      arr += MAX_SZ;
      /* call into the MLRISC glue code that will make the C call */
      int tmp = mlriscGlue(arr0);
      printf("%d", tmp);
 }

 /* main.c */
 int main () 
 {
     glueCode();
     return 0;
 }

 /* sanity.c */
 int main ()
 {
     int x = 23432;
     float y = 1024.013f;
     int tmp = target(x, y);
     printf ("%d", tmp);
     return 0;
 }

 * We also generate the mlriscGlue code in MLRISC.  This code grabs the arguments from
 * arr0 and then passes them to target using Staged Allocation.
 * 
 *)

functor GenTestFn(
	structure T : MLTREE
		where type Region.region = unit
	structure Cells : CELLS
	structure CCall : C_CALL
                where T = T
	val codegen : (string * string * CType.c_proto * T.stm list * CCall.c_arg list) -> unit
	val param0 : T.rexp
	val wordTy : int
  ) =
  struct

    structure CTy = CType

    val retValVar = "retVal"

    val mem = ()

    val wordSzB = wordTy div 8

    fun zip3 (ls1, ls2, ls3) = let
	fun f ([], _, _, xs) = List.rev xs
	  | f (x1 ::xs1, x2::xs2, x3::xs3, xs) = f(xs1, xs2, xs3, (x1, x2, x3) :: xs)
        in
	   f(ls1, ls2, ls3, [])
        end

    datatype c_argument =
	     INT of int
	   | SHORT of int
	   | CHAR of int
	   | FLOAT of real
	   | DOUBLE of real
	   | POINTER of int
	   | STRUCT of c_argument list

    fun flattenArg cArg = (case cArg
        of STRUCT args => List.concat (List.map flattenArg args)
	 | cArg => [cArg])

    fun cArgToString (cArg) = (case cArg
        of INT i => Int.toString i
	 | SHORT i => Int.toString i
	 | CHAR c => Int.toString c
	 | FLOAT f => Real.toString f^"f"
	 | DOUBLE f => Real.toString f
	 | POINTER p => "(void*)0x"^Int.fmt StringCvt.HEX p
	 | STRUCT args => "{"^String.concatWith ", " (List.map cArgToString args)^"}"
        (* end case *))

    fun tyToFormatString (ty) = (case ty
        of CTy.C_signed CTy.I_char => "%c"
	 | CTy.C_unsigned CTy.I_char => "%c"
	 | CTy.C_unsigned _ => "%u"
	 | CTy.C_signed _ => "%d"
	 | CTy.C_float => "%f"
	 | CTy.C_double => "%f"
	 | CTy.C_PTR => "%p"
	 | CTy.C_STRUCT cTys => String.concatWith " " (List.map tyToFormatString cTys)
        (* end case *))

    fun cTyToName cTy = (case cTy
        of CTy.C_unsigned CTy.I_int => "u"
	 | CTy.C_signed CTy.I_int => "i"
	 | CTy.C_signed CTy.I_char => "c"
	 | CTy.C_unsigned CTy.I_char => "c"
	 | CTy.C_signed CTy.I_short => "i"
	 | CTy.C_unsigned CTy.I_short => "u"
	 | CTy.C_float => "f"
	 | CTy.C_double => "d"
	 | CTy.C_PTR => "vs"
	 | CTy.C_STRUCT cTys => "s"^String.concat (List.map cTyToName cTys)
        (* end case *))

    fun cTyToString (ty) = (case ty
        of CTy.C_unsigned CTy.I_int => "unsigned int"
	 | CTy.C_signed CTy.I_int => "int"
	 | CTy.C_unsigned CTy.I_char => "char"
	 | CTy.C_signed CTy.I_char => "char"
	 | CTy.C_signed CTy.I_short => "short"
	 | CTy.C_unsigned CTy.I_short => "short"
	 | CTy.C_float => "float"
	 | CTy.C_double => "double"
	 | CTy.C_PTR => "void*"
	 | CTy.C_void => "void"
	 | CTy.C_STRUCT cTys => "struct "^cTyToName ty
        (* end case *))

    val i = ref 0
    fun freshName () = (
	i := (!i) + 1;
	"x"^Int.toString (!i))

    fun cTyDecl' cTy = (case cTy
         of CTy.C_STRUCT cTys => cTyToString cTy^"{ "^(String.concatWith " " (List.map cTyDecl' cTys))^"}"^freshName()^";"
	 | cTy => cTyToString cTy^" "^freshName()^";"
        (* end case *))
		       
    fun cTyDecl cTy = let
	val td = cTyDecl' cTy
        in 
	    i := 0;
	    td
        end

    fun cTyNames' prefix (cTy) = (case cTy
         of CTy.C_STRUCT cTys => List.concat (List.map (cTyNames' (prefix^"."^freshName())) cTys)
	 | cTy => [prefix^"."^freshName()]
        (* end case *))
		       
    fun cTyNames prefix cTy = let
	val td = (case cTy
            of CTy.C_STRUCT cTys => List.concat (List.map (cTyNames' prefix) cTys)
	     | cTy => [prefix])
        in 
	    i := 0;
	    td
        end

    fun genParamName i = 
	" a"^Int.toString i

  (* generate parameter names for a list of types *)
    fun genParamNames tys = 
	List.rev(#2 (List.foldl (fn (ty, (i, params)) => (i+1, genParamName(i) :: params)) (0, []) tys))

  (* generate formal parameters for a list of types and variables *)
    fun genFormals (paramTys, vars) = let
        fun f (ty, name) = cTyToString ty^name
        in
           List.map f (ListPair.zip (paramTys, vars))
        end

    fun genTargetPrintfArgs (vars, paramTys) = let
	fun f (prefix, ty) = cTyNames prefix ty
        in
	   List.concat (ListPair.map f (vars, paramTys))
        end

    fun genPrintf (formatString, args) =
	"printf("^String.concatWith "," (formatString :: args)^");"

    fun genFormatString (elts) =
	"\"" ^ String.concatWith " " elts ^ "\\n\""

    fun fmtVar (arg, ty) = arg^"="^tyToFormatString ty^"\\n"

  (* construct a format string printing the parameters of a proto *)
    fun protoToFormatString ({conv, retTy, paramTys}, args) = 
	   genFormatString (ListPair.map fmtVar (args, paramTys))

    fun protoToPrintf (proto, args) = genPrintf(protoToFormatString(proto, args), args)	

    (* generate a dummy target function that prints its parameters *)
    fun targetFun (targetName, proto as {conv, retTy, paramTys}, retVal) = let
	val vars = genParamNames paramTys
	val params = genFormals(paramTys, vars)
	val printfArgs = genTargetPrintfArgs(vars, paramTys)
	in 
           cTyToString retTy ^ " " ^ targetName ^ "(" ^ (String.concatWith ", " params) ^ ")" ^
	      "{" ^
	          protoToPrintf(proto, printfArgs) ^
	          "return "^(String.concat (List.map cArgToString retVal))^";"^
              "}"
        end

    fun genMLRISCGlueHdr (mlriscGlue, proto as {conv, retTy, paramTys}) = 
        cTyToString retTy ^ " " ^ mlriscGlue ^ "(void* arr0);"

    fun genAssignArg (ty, var, arg) = cTyToString(ty)^" "^var^" = "^cArgToString(arg)^";"

    (* generate C code that initializes an argument in arr0 *) 
    fun genInitArr0 (ty, arg) = 
	 String.concatWith "\t" [
         "{",
              genAssignArg(ty, "tmp", arg),
	      "memcpy(arr, &tmp, sizeof("^cTyToString(ty)^"));",
	      "arr += MAX_SZ;",
         "}\n"
         ]

    (* generate C code that calls the MLRISC-generated function *)
    fun genCGlueCode (mlriscGlue, proto as {conv, retTy, paramTys}, args) = let
	val stms = List.rev (ListPair.map genInitArr0 (paramTys, args))
	val glueCall = if retTy <> CTy.C_void
                          then cTyToString retTy ^ " " ^retValVar^" = " ^ mlriscGlue^"(arr0);\n\t"^
			       genPrintf(genFormatString([tyToFormatString retTy]), [retValVar])
		          else mlriscGlue^"(arr0);"
        in
	   String.concatWith "\n\t" [
             "void glueCode(){",
                (* initialize arguments *)
                "void* arr0[4096];",
                "void** arr = arr0;",
                String.concatWith "\t " (List.rev stms),
	        glueCall,
	      "}"
            ]
        end

    (* generate C code that calls the MLRISC-generated function *)
    fun genTestCode (proto as {conv, retTy, paramTys}, args) = let
	val paramNames = genParamNames paramTys
	val stms = List.map genAssignArg (zip3(paramTys, paramNames, args))
	val callTarget = "target("^String.concatWith ", " paramNames^");"
	val glueCall = if retTy <> CTy.C_void
                          then cTyToString retTy ^ " " ^retValVar^" = " ^ callTarget^"\n\t"^
			       genPrintf(genFormatString([tyToFormatString retTy]), [retValVar])
		          else callTarget
        in
	   String.concatWith "\n\t" [
             "void testCode(){",
                String.concatWith "\t " (List.rev stms),
	        glueCall,
	      "}"
            ]
        end

    val cIncludes = String.concatWith "\n" [
        "#include <stdio.h>",
        "#include <stdlib.h>",
        "#include <string.h>\n"
    ]

    fun isStruct (CTy.C_STRUCT _) = true
      | isStruct _ = false

  (* number of bytes to represent ty *)
    fun szOfTy ty = if ty = CTy.C_void then 0 else #sz (CSizes.sizeOfTy ty)

  (* returns the maximum size type for a C prototype *)
    fun maxSzOfProto ({conv, paramTys, retTy}) = 
           List.foldl Int.max 0 (List.map szOfTy (retTy :: paramTys))

    fun genGlue (target, mlriscGlue, proto, args, retVal) = String.concatWith "\n" [
          cIncludes,
  	  "#define MAX_SZ "^Int.toString(Int.max(1,(maxSzOfProto proto) div wordSzB)),
	  (* tyep declarations for structs *)
	  String.concatWith "\n" (List.map cTyDecl (List.filter isStruct (#paramTys proto))),
	  (* C prototype for the MLRISC assembly stub *)
	  genMLRISCGlueHdr(mlriscGlue, proto),
	  (* target function *)
	  targetFun(target, proto, retVal),
	  (* C glue code for calling into the MLRISC assembly stub *)
	  genCGlueCode(mlriscGlue, proto, args),
	  (* C test code that directly calls the target function *)
	  genTestCode(proto, args)
        ]

    fun genCMain () = "int main () { glueCode(); return 0; }"

    fun genSanityCheck (proto, args, retVal) = "int main () { testCode(); return 0; }"

    fun offset szB arr0 i = T.ADD(wordTy, arr0, T.LI (T.I.fromInt (wordTy, i*szB)))

  (* generate the ith argument to the MLRISC code *)
    fun genGlueArg szB arr0 (ty, (i, args)) = (i+1, 
	(case ty
          of CTy.C_signed CTy.I_int => CCall.ARG (T.LOAD(32, offset szB arr0 i, mem))
	   | CTy.C_unsigned CTy.I_int => CCall.ARG (T.LOAD(32, offset szB arr0 i, mem))
	   | CTy.C_unsigned CTy.I_char => CCall.ARG (T.LOAD(8, offset szB arr0 i, mem))
	   | CTy.C_signed CTy.I_char => CCall.ARG (T.LOAD(8, offset szB arr0 i, mem))
	   | CTy.C_PTR => CCall.ARG (T.LOAD(wordTy, offset szB arr0 i, mem))
	   | CTy.C_STRUCT _ => CCall.ARG (T.LOAD(wordTy, offset szB arr0 i, mem))
	   | CTy.C_float => CCall.FARG (T.FLOAD(32, offset szB arr0 i, mem))
	   | CTy.C_double => CCall.FARG (T.FLOAD(64, offset szB arr0 i, mem))
        (* end case *)) :: args)

    val rand = Random.rand (0, 255)

    fun genRandArg ty = (case ty
        of CTy.C_float => FLOAT (Random.randReal(rand))
	 | CTy.C_double => DOUBLE(Random.randReal(rand))
	 | CTy.C_unsigned CTy.I_int => INT (Random.randNat(rand))
	 | CTy.C_signed CTy.I_int => INT (Random.randNat(rand))
	 | CTy.C_signed CTy.I_char => CHAR (Random.randNat rand mod 255)
	 | CTy.C_PTR => POINTER(Random.randNat(rand))
	 | CTy.C_STRUCT cTys => STRUCT(List.map genRandArg cTys)
        (* end case *))

    fun output (strm, s) = TextIO.output(strm, s^"\n")

  (* test parameter types *)
    val pty1 = [CTy.C_double, CTy.C_unsigned CTy.I_int, CTy.C_PTR, CTy.C_double, 
		CTy.C_float, CTy.C_PTR, CTy.C_float, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR,
		CTy.C_signed CTy.I_int, 
		CTy.C_double, CTy.C_double, CTy.C_double, CTy.C_double, CTy.C_double, 
		CTy.C_double, CTy.C_double]
    val pty2 = [CTy.C_STRUCT [CTy.C_float]]
    val pty3 = [CTy.C_STRUCT [CTy.C_float,CTy.C_float]]
    val pty3 = [CTy.C_STRUCT [CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float]]
    val pty4 = [CTy.C_STRUCT [CTy.C_PTR,CTy.C_float,CTy.C_float,CTy.C_float]]
    val pty5 = [CTy.C_double, CTy.C_unsigned CTy.I_int, CTy.C_PTR]
    val pty6 = [CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR]
    val pty7 = [CTy.C_float]
    val pty7 = [CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float]
    val pty8 = [CTy.C_PTR]
    val pty9 = [CTy.C_signed CTy.I_int]
    val pty10 = [CTy.C_signed CTy.I_int, CTy.C_signed CTy.I_int]
    val pty11 = [CTy.C_signed CTy.I_int, CTy.C_signed CTy.I_char, CTy.C_float]
    val pty12 = [CTy.C_STRUCT [CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR]]
    val pty13 = [CTy.C_float, CTy.C_double]

    fun main _ = (*BackTrace.monitor (fn () => *) (let
      (* choose the prototype to test *)
	val retTy = CTy.C_void
	val paramTys = pty5

	val cArgs = List.map genRandArg paramTys
	val retVal = if retTy <> CTy.C_void then [genRandArg retTy] else []
	val proto = {conv="ccall", retTy=retTy, paramTys=paramTys}
	val mlriscGlue = "mlriscGlue"
	val target = "target"

	(* output C code that glues to the MLRISC code  *)
	val cOutStrm = TextIO.openOut "glue.c"
	val cCode = genGlue(target, mlriscGlue, proto, cArgs, retVal)
	val _ = output(cOutStrm, cCode)
	val _ = TextIO.closeOut cOutStrm

	(* output C code for santity check *)
	val cOutStrm = TextIO.openOut "sanity.c"
	val cCode = genSanityCheck(proto, cArgs, retVal)
	val _ = output(cOutStrm, cCode)
	val _ = TextIO.closeOut cOutStrm

	(* output main *)
	val cMainOutStrm = TextIO.openOut "main.c"
	val cMain = genCMain()
	val _ = output(cMainOutStrm, cMain)
	val _ = TextIO.closeOut cMainOutStrm

	(* output MLRISC code *)
	val tmpReg = Cells.newReg()
	val tmpR = T.REG(wordTy, tmpReg)
	val szB = Int.max(wordSzB, maxSzOfProto proto)
	val (_, glueArgs) = List.foldl (genGlueArg szB tmpR) (0, []) paramTys
	val asmOutStrm = TextIO.openOut "mlrisc.s"
	fun doit () = codegen(mlriscGlue, target, proto, [T.MV(32, tmpReg, param0)], List.rev glueArgs)
	val _ = AsmStream.withStream asmOutStrm doit ()
	val _ = TextIO.closeOut asmOutStrm
	in          
	  0
        end)

  end
