(*
 * WARNING: This file was automatically generated by MDLGen (v3.1)
 * from the machine description file "ppc/ppc.mdl".
 * DO NOT EDIT this file directly
 *)


signature PPCCELLS =
sig
   include CELLS
   val SPR : CellsBasis.cellkind
   val CELLSET : CellsBasis.cellkind
   val showGP : CellsBasis.register_id -> string
   val showFP : CellsBasis.register_id -> string
   val showCC : CellsBasis.register_id -> string
   val showSPR : CellsBasis.register_id -> string
   val showMEM : CellsBasis.register_id -> string
   val showCTRL : CellsBasis.register_id -> string
   val showCELLSET : CellsBasis.register_id -> string
   val showGPWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showFPWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showCCWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showSPRWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showMEMWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showCTRLWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showCELLSETWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val r0 : CellsBasis.cell
   val xer : CellsBasis.cell
   val lr : CellsBasis.cell
   val ctr : CellsBasis.cell
   val addGP : CellsBasis.cell * cellset -> cellset
   val addFP : CellsBasis.cell * cellset -> cellset
   val addCC : CellsBasis.cell * cellset -> cellset
   val addSPR : CellsBasis.cell * cellset -> cellset
   val addMEM : CellsBasis.cell * cellset -> cellset
   val addCTRL : CellsBasis.cell * cellset -> cellset
   val addCELLSET : CellsBasis.cell * cellset -> cellset
end

structure PPCCells : PPCCELLS =
struct
   exception PPCCells
   fun error msg = MLRiscErrorMsg.error("PPCCells",msg)
   open CellsBasis
   fun showGPWithSize (r, ty) = (fn (r, _) => (if ( ! PPCAsmSyntax.ibm_syntax)
                                       then (Int.toString r)
                                       else ("r" ^ (Int.toString r)))
                                ) (r, ty)
   and showFPWithSize (r, ty) = (fn (f, _) => (if ( ! PPCAsmSyntax.ibm_syntax)
                                       then (Int.toString f)
                                       else ("f" ^ (Int.toString r)))
                                ) (r, ty)
   and showCCWithSize (r, ty) = (fn (cr, _) => "cr" ^ (Int.toString cr)
                                ) (r, ty)
   and showSPRWithSize (r, ty) = (fn (1, _) => "xer"
                                   | (8, _) => "lr"
                                   | (9, _) => "ctr"
                                   | (r, _) => Int.toString r
                                 ) (r, ty)
   and showMEMWithSize (r, ty) = (fn (r, _) => "m" ^ (Int.toString r)
                                 ) (r, ty)
   and showCTRLWithSize (r, ty) = (fn (r, _) => "ctrl" ^ (Int.toString r)
                                  ) (r, ty)
   and showCELLSETWithSize (r, ty) = (fn _ => "CELLSET"
                                     ) (r, ty)
   fun showGP r = showGPWithSize (r, 64)
   fun showFP r = showFPWithSize (r, 64)
   fun showCC r = showCCWithSize (r, 4)
   fun showSPR r = showSPRWithSize (r, 64)
   fun showMEM r = showMEMWithSize (r, 8)
   fun showCTRL r = showCTRLWithSize (r, 8)
   fun showCELLSET r = showCELLSETWithSize (r, 0)
   val SPR = CellsBasis.newCellKind {name="SPR", nickname="spr"}
   and CELLSET = CellsBasis.newCellKind {name="CELLSET", nickname="cellset"}
   structure MyCells = Cells
      (exception Cells = PPCCells
       val firstPseudo = 256
       val desc_GP = CellsBasis.DESC {low=0, high=31, kind=CellsBasis.GP, defaultValues=[],
              zeroReg=NONE, toString=showGP, toStringWithSize=showGPWithSize,
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_FP = CellsBasis.DESC {low=32, high=63, kind=CellsBasis.FP,
              defaultValues=[], zeroReg=NONE, toString=showFP, toStringWithSize=showFPWithSize,
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_CC = CellsBasis.DESC {low=64, high=71, kind=CellsBasis.CC,
              defaultValues=[], zeroReg=NONE, toString=showCC, toStringWithSize=showCCWithSize,
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_SPR = CellsBasis.DESC {low=72, high=103, kind=SPR, defaultValues=[],
              zeroReg=NONE, toString=showSPR, toStringWithSize=showSPRWithSize,
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_MEM = CellsBasis.DESC {low=104, high=103, kind=CellsBasis.MEM,
              defaultValues=[], zeroReg=NONE, toString=showMEM, toStringWithSize=showMEMWithSize,
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_CTRL = CellsBasis.DESC {low=104, high=103, kind=CellsBasis.CTRL,
              defaultValues=[], zeroReg=NONE, toString=showCTRL, toStringWithSize=showCTRLWithSize,
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_CELLSET = CellsBasis.DESC {low=104, high=103, kind=CELLSET,
              defaultValues=[], zeroReg=NONE, toString=showCELLSET, toStringWithSize=showCELLSETWithSize,
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       val cellKindDescs = [(CellsBasis.GP, desc_GP), (CellsBasis.FP, desc_FP),
              (CellsBasis.CC, desc_CC), (SPR, desc_SPR), (CellsBasis.MEM, desc_MEM),
              (CellsBasis.CTRL, desc_CTRL), (CELLSET, desc_CELLSET)]
       val cellSize = 8
      )

   open MyCells
   val addGP = CellSet.add
   and addFP = CellSet.add
   and addCC = CellSet.add
   and addSPR = CellSet.add
   and addMEM = CellSet.add
   and addCTRL = CellSet.add
   and addCELLSET = CellSet.add
   val RegGP = Reg GP
   and RegFP = Reg FP
   and RegCC = Reg CC
   and RegSPR = Reg SPR
   and RegMEM = Reg MEM
   and RegCTRL = Reg CTRL
   and RegCELLSET = Reg CELLSET
   val stackptrR = RegGP 1
   val asmTmpR = RegGP 28
   val fasmTmp = RegFP 0
   val r0 = RegGP 0
   val xer = RegSPR 1
   val lr = RegSPR 8
   val ctr = RegSPR 9
end

