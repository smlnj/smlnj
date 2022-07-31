(* gen-fn.sml
 *
 * Generate the interpreter loop.
 *) 

functor GenFn (
    structure T : MLTREE
    structure CCallGen : C_CALL_GEN
      where T = T
  (* general-purpose registers used for passing or returning arguments *)
    val gprs : T.reg list
  (* floating-point registers used for passing or returning arguments *)
    val fprs : T.reg list
  (* possible widths *)
    val gprWidths : T.ty list
    val fprWidths : T.ty list
  (* stack pointer register *)
    val spReg : T.rexp
    val defaultWidth : T.ty
    val callerSaves : T.reg list
    val callerSavesF : T.reg list
  ) :> sig

  (* generate the machine-independent part of the vararg interpreter *)
    val gen : {interpFunPtr : T.rexp, largsReg : T.reg, endOfLargs : T.rexp} -> T.stm list

  end = struct

    structure T = CCallGen.T
    structure Consts = VarargConstants
    structure SA = CCallGen.SA

    datatype loc 
      = REG_LOC of T.reg
      | STK_LOC

    datatype loc_kind = datatype CLocKind.loc_kind

  (* as we go from top to bottom, we become increasingly specific about the destination of the argument. *)
    datatype branch
      = ENTRY of {larg : T.rexp, ks : loc_kind list, widths : T.ty list, narrowings : T.ty list, locs : loc list}
      | KIND of {larg : T.rexp, k : loc_kind, widths : T.ty list, narrowings : T.ty list, locs : loc list}
      | WIDTH of {larg : T.rexp, k : loc_kind, width : T.ty, narrowings : T.ty list, locs : loc list}
      | NARROWING of {larg : T.rexp, k : loc_kind, width : T.ty, narrowing : T.ty, locs : loc list}
      | LOC of {larg : T.rexp, k : loc_kind, width : T.ty, narrowing : T.ty, loc : loc}

    val regToInt = CellsBasis.physicalRegisterNum
    fun locToInt (REG_LOC r) = regToInt r
      | locToInt STK_LOC = 0

  (* labels *)
    local
	val instLabels = ref ([] : (string * Label.label) list)
	fun newLabel s = (case List.find (fn (s', _) => s' = s) (!instLabels)
                of NONE => let
	           val l = Label.label (s^"L") ()
		   in
		       instLabels := (s, l) :: !instLabels;
		       l
		   end
		 | SOME (s, l) => l
               (* end case *))
	fun kindToString GPR = "GPR"
	  | kindToString FPR = "FPR"
	  | kindToString STK = "STK"
	  | kindToString FSTK = "FSTK"
	val c = String.concatWith "."
	val i2s = Int.toString
	fun locToString (REG_LOC r) = "r"^i2s (regToInt r)
	  | locToString STK_LOC = "stk"
	fun instToString (ENTRY {...}) = "entry"
	  | instToString (KIND {k, ...}) = c["kind", kindToString k]
	  | instToString (WIDTH {k, width, ...}) = c["width", kindToString k, i2s width]
	  | instToString (NARROWING {k, width, narrowing, ...}) = 
	        c["narrowing", kindToString k, i2s width, i2s narrowing]
	  | instToString (LOC {k, width, narrowing, loc, ...}) = 
	        c["loc", kindToString k, i2s width, i2s narrowing, locToString loc]
    in
  (* generates labels for instructions *)
    val labelOfInst = newLabel o instToString
    val interpEntryLab = newLabel "interpEntry"
    val interpLab = newLabel "interp"
    val gotoCLab = newLabel "gotoC"
    val errLab = Label.global "vararg_error"
    end (* local *)

    val defTy = defaultWidth
    val mem = T.Region.memory
    val stack = T.Region.stack
    fun lit i = T.LI (T.I.fromInt (defTy, i))
    val lit' = lit o Word32.toInt
    fun gpr r = T.GPR (T.REG (defTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))
    fun concatMap f xs = List.concat (List.map f xs)

  (* displacement from the located argument *)
    fun offLocdArg (ty, larg, off) = T.LOAD(ty, T.ADD(defTy, larg, lit' off), mem)
    fun offLocdArgF (ty, larg, off) = T.FLOAD(ty, T.ADD(defTy, larg, lit' off), mem)

  (* store an integer argument on the stack *)
    fun storeSTK larg ty = 
	    T.STORE(ty, T.ADD (defTy, spReg, offLocdArg(defTy, larg, Consts.locOffB)), 
		    offLocdArg(ty, larg, Consts.argOffB), mem)

  (* store a floating-point argument on the stack *)
    fun storeFSTK larg ty = 
	    T.FSTORE(ty, T.ADD (defTy, spReg, offLocdArg(defTy, larg, Consts.locOffB)), 
		    offLocdArgF(ty, larg, Consts.argOffB), mem)

  (* are the width and narrowing legal for kind of location? *)
    fun widthOK (k, w, narrowing) = let
	    val ws = (case k
		       of (GPR | STK) => gprWidths
			| (FPR | FSTK) => fprWidths)
            in
	       List.exists (fn w' => w = w') ws andalso List.exists (fn w' => narrowing = w') ws
	    end

  (* generate code that places the argument *)
    fun loc {larg, k : CLocKind.loc_kind, width, narrowing, loc} = let
          (* offset into the argument (only nonzero if the argument has an aggregate type) *)
	    val argMembOff = offLocdArg(width, larg, Consts.offsetOffB)
          (* narrow the location if necessary *)
	    fun narrow loc = if width = narrowing then loc
			     else SA.NARROW(loc, width, k)
	    val writeArgInstrs = (
		case (k, loc, widthOK(k, width, narrowing))
		 of (GPR, REG_LOC r, true) => 
		    CCallGen.writeLoc (CCallGen.ARG (offLocdArg(width, larg, Consts.argOffB)))
				      (argMembOff, narrow(SA.REG(width, GPR, r)), [])
		  | (FPR, REG_LOC r, true) =>
		    CCallGen.writeLoc (CCallGen.FARG (offLocdArgF(width, larg, Consts.argOffB)))
				      (argMembOff, narrow(SA.REG(width, FPR, r)), [])
		  | (STK, STK_LOC, true) =>
		    [storeSTK larg width]
		  | (FSTK, STK_LOC, true) =>
		    [storeFSTK larg width]
		  | _ => [T.JMP (T.LABEL errLab, [])]
               (* end case *))
            in
		 (* instructions to write the argument to the location *)
		   writeArgInstrs @
		 (* return to the interpreter loop *)
		   [T.JMP (T.LABEL interpLab, [])]
	    end

    fun genHandlers (i, f, instrs) = let
	    fun genHandler instr = let
		    val lab = labelOfInst (i instr)
		    in
                      List.concat [
	               [T.DEFINE lab],
		       f instr,
		       [T.JMP (T.LABEL errLab, [])]
		    ]    
	           end
            in
	       concatMap genHandler instrs
	    end

  (* generate code to handle an argument narrowing *)
    fun narrowing {larg, k, width, narrowing, locs} = let
	  (* we only use this instruction for generating labels *)
	    fun branch loc = LOC {larg=larg, k=k, width=width, narrowing=narrowing, loc=loc}
	    val locBranches = List.map (labelOfInst o branch) locs
	    fun instr (loc, branch) = if (k = GPR orelse k = FPR)
                    then T.BCC(T.CMP(defTy, T.EQ, 
				     offLocdArg(defTy, larg, Consts.locOffB),
				     lit (locToInt loc)),
			       branch)
                    else T.JMP (T.LABEL branch, [])
            in
	       ListPair.map instr (locs, locBranches)
	    end

  (* generate code to handle an argument width *)
    fun width {larg, k, width, narrowings, locs} = let
	  (* we only use this instruction for generating labels *)
	    fun branch narrowing = NARROWING {larg=larg, k=k, width=width, narrowing=narrowing, locs=locs}
	    val narrowingBranches = List.map (labelOfInst o branch) narrowings
	    fun instr (narrowing, branch) =
		    T.BCC(T.CMP(defTy, T.EQ, 
				offLocdArg(defTy, larg, Consts.narrowingOffB),
				lit narrowing),
			  branch)
            in
	       ListPair.map instr (narrowings, narrowingBranches)
	    end

  (* generate code to handle an argument kind *)
    fun kind {larg, k, widths, narrowings, locs} = let
	    fun branch width = WIDTH {larg=larg, k=k, width=width, narrowings=narrowings, locs=locs}
	    val widthBranches = List.map (labelOfInst o branch) widths
	    fun instr (width, branch) =
		    T.BCC(T.CMP(defTy, T.EQ, 
				offLocdArg(defTy, larg, Consts.widthOffB),
				lit width),
			  branch)
            in
	       ListPair.map instr (widths, widthBranches)
	    end

  (* generate code to handle an argument kind *)
    fun entry {larg, ks, widths, narrowings, locs} = let
	    fun branch k = KIND {larg=larg, k=k, widths=widths, narrowings=narrowings, locs=locs}
	    val kBranches = List.map (labelOfInst o branch) ks
	    fun instr (k, branch) =
		    T.BCC(T.CMP(defTy, T.EQ, 
				offLocdArg(defTy, larg, Consts.kindOffB),
				lit'(Consts.kind k)),
			  branch)
            in
	       ListPair.map instr (ks, kBranches)
	    end

    fun locInstrs {larg, k, width, narrowing, locs=[]} = []
      | locInstrs {larg, k, width, narrowing, locs=loc::locs} =
	    {larg=larg, k=k, width=width, narrowing=narrowing, loc=loc} :: 
	      locInstrs {larg=larg, k=k, width=width, narrowing=narrowing, locs=locs}

    fun widthOK ((STK | GPR), width) = List.exists (fn width' => width' = width) gprWidths
      | widthOK ((FSTK | FPR), width) = List.exists (fn width' => width' = width) fprWidths

    fun narrowingInstrs {larg, k, width, narrowings=[], locs} = []
      | narrowingInstrs {larg, k, width, narrowings=narrowing::narrowings, locs} = let
	    val instrs = narrowingInstrs {larg=larg, k=k, width=width, narrowings=narrowings, locs=locs}
	    in
	      if widthOK(k, narrowing)
	         then {larg=larg, k=k, width=width, narrowing=narrowing, locs=locs} :: instrs
	      else instrs
	    end
	      

    fun widthInstrs {larg, k, widths=[], narrowings, locs} = []
      | widthInstrs {larg, k, widths=width::widths, narrowings, locs} = let
	    val instrs = widthInstrs {larg=larg, k=k, widths=widths, narrowings=narrowings, locs=locs}
	    in
              if widthOK(k, width)
	      then 
		{larg=larg, k=k, width=width, narrowings=narrowings, locs=locs} :: instrs
	      else instrs
            end		

    fun kindInstrs {larg, ks=[], widths, narrowings, locs} = []
      | kindInstrs {larg, ks=k::ks, widths, narrowings, locs} = 
	    {larg=larg, k=k, widths=widths, narrowings=narrowings, locs=locs} :: 
	      kindInstrs {larg=larg, ks=ks, widths=widths, narrowings=narrowings, locs=locs}

    structure IS = IntBinarySet
    fun mkUnique ints = IS.listItems(IS.addList(IS.empty, ints))

    fun entryInstr larg = let
	    val ks = [GPR, FPR, STK, FSTK]
	    val widths = mkUnique (gprWidths@fprWidths)
	    val narrowings = widths
	    val locs = STK_LOC :: List.map REG_LOC gprs @ List.map REG_LOC fprs
            in
	       {larg=larg, ks=ks, widths=widths, narrowings=narrowings, locs=locs}
	    end

  (* all possible combinations of instructions *)
    fun allInstrs larg = let
	    val entryInstr = entryInstr larg
	    val kindInstrs = kindInstrs entryInstr
	    val widthInstrs = concatMap widthInstrs kindInstrs
	    val narrowingInstrs = concatMap narrowingInstrs widthInstrs
	    val locInstrs = concatMap locInstrs narrowingInstrs 
            in
	         (entryInstr, kindInstrs, widthInstrs, narrowingInstrs, locInstrs)
	    end

  (* call the varargs C function *)
    fun genCallC interpFunPtr = let
	   val defs = List.map gpr callerSaves @ List.map (fn r => fpr(64, r)) callerSavesF
	   val uses = List.map gpr gprs @ List.map (fn r => fpr(64, r)) fprs
	   in
	      [
	       T.DEFINE gotoCLab,
	       T.CALL {funct=interpFunPtr, targets=[], defs=defs, uses=uses, region=mem, pops=0}
	      ]
	   end

  (* interpreter for varlargs *)
    fun genInterp (largs, largsReg, endOfLargs) = [
            T.DEFINE interpLab,
	  (* loop through the largs *)
	    T.MV (defTy, largsReg, T.ADD (defTy, largs, lit' Consts.locdArgSzB)),
	    T.DEFINE interpEntryLab,
	    T.BCC (T.CMP(defTy, T.GE, largs, endOfLargs), gotoCLab)
          ]

    fun gen {interpFunPtr, largsReg, endOfLargs} = let           
	    val largs = T.REG (defTy, largsReg)
	    val (entryInstr, kindInstrs, widthInstrs, narrowingInstrs, locInstrs) = allInstrs largs
            in
	      List.concat [
	         [T.JMP (T.LABEL interpEntryLab, [])],
	         genInterp(largs, largsReg, endOfLargs),
		 genHandlers(ENTRY, entry, [entryInstr]),
		 genHandlers(KIND, kind, kindInstrs), 
		 genHandlers(WIDTH, width, widthInstrs), 
		 genHandlers(NARROWING, narrowing, narrowingInstrs),
		 genHandlers(LOC, loc, locInstrs), 
		 genCallC interpFunPtr
	      ]
	    end

  end (* GenFn *)
