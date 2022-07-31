(*
 * Generate the <arch>DelaySlots functor.
 * This structure contains information about delay slot filling 
 *)

functor MDGenDelaySlots(Comp : MDL_COMPILE) : MDL_GEN_MODULE =
struct

   structure Comp = Comp
   structure Ast  = Comp.Ast
   structure Env  = Comp.Env

   open Ast Comp.Util

   fun delay DELAY_NONE = ID "D_NONE"
     | delay DELAY_ERROR = ID "D_ERROR"
     | delay DELAY_ALWAYS = ID "D_ALWAYS"
     | delay DELAY_TAKEN = ID "D_TAKEN"
     | delay DELAY_NONTAKEN = ID "D_FALLTHRU"
     | delay(DELAY_IF(BRANCHforwards,x,y)) = 
         IFexp(ID "backward",delay y,delay x)
     | delay(DELAY_IF(BRANCHbackwards,x,y)) = 
         IFexp(ID "backward",delay x,delay y)
   and flag FLAGoff = BOOLexp false
     | flag FLAGon  = BOOLexp true
     | flag(FLAGid(id,true,e)) = ANDALSO(ID id,e)
     | flag(FLAGid(id,false,e)) = ANDALSO(APP("not",ID id),e)
   fun delaySlotEntry(nop,n,nOn,nOff) =
       RECORDexp[ ("nop",nop), ("n",n), ("nOn",nOn), ("nOff",nOff) ]
   val defaultDelaySlot =
        delaySlotEntry(TRUE,FALSE,delay DELAY_ERROR,delay DELAY_NONE)

   fun gen md =
   let (* Name of the functor and its signature *)
       val strName = Comp.strname md "DelaySlots" 
       val sigName = "DELAY_SLOT_PROPERTIES"

       (* The instruction set *)
       val instructions = Comp.instructions md

       (* The environment *)
       val env = Env.empty 

       (* Arguments to the functor *)
       val args =
           ["structure I : "^Comp.strname md "INSTR",
            "structure P : INSN_PROPERTIES", 
            "   where I = I"
           ]

       fun mkFun(name,args,x,body,default) = 
           FUNdecl[FUNbind(name,
              [CLAUSE([RECORDpat(map (fn x => (x,IDpat x)) args, NONE, false)],
               LETexp([FUNdecl
                         [FUNbind(name,[CLAUSE([IDpat x],
                                        NONE,
                                        CASEexp(ID x,
                                            body @ 
                                            [CLAUSE([WILDpat],NONE,default)]
                          ))])]],
                      [APPexp(ID name,ID x)]))
              ])]

       (* Function to extract the properties about delay slot *)
       val delaySlot = 
           let fun mkPat cons = Env.consToPat {prefix="I",cons=cons}
               fun g [] = []
                 | g(CONSbind{delayslot=(_,DELAY_NONE),
                                  nop=FLAGoff,nullified=FLAGoff, ...}::cbs) = 
                     g cbs
                 | g((c as CONSbind{id,delayslot=(d1,d2),
                                    nop,nullified,...})::cbs) = 
                     CLAUSE([mkPat c],
                            NONE,
                            delaySlotEntry(flag nop, flag nullified,
                                                   delay d1,delay d2))::g cbs
           in  mkFun("delaySlot",["instr","backward"],"instr",g instructions,
                     defaultDelaySlot)
           end

       (* Function to enable/disable a delay slot *)
       val enableDelaySlot = DUMMYfun "enableDelaySlot"

       (* Function to check whether two delay slots have conflicts *)
       val conflict = DUMMYfun "conflict"

       (* Function to check a instruction is a delay slot candidate *)
       val delaySlotCandidate = 
           let fun g [] = []
                 | g(CONSbind{delaycand=NONE, ...}::cbs) = g cbs
                 | g((c as CONSbind{delaycand=SOME e, ...})::cbs) = 
                     CLAUSE([Env.consToPat {prefix="I",cons=c}],NONE,e)::g cbs
           in  mkFun("delaySlotCandidate",
                     ["jmp","delaySlot"],"delaySlot",g instructions,TRUE)
           end

       (* Function to set the target of a branch *)
       val setTarget = DUMMYfun "setTarget"
 
       (* The functor *)
       val strBody = 
           [$ ["structure I = I",
               "datatype delay_slot = D_NONE | D_ERROR | D_ALWAYS | D_TAKEN | D_FALLTHRU ",
               ""
              ],
              ERRORfun strName,
              delaySlot,
              enableDelaySlot,
              conflict,
              delaySlotCandidate,
              setTarget
           ]

   in  Comp.codegen md "backpatch/DelaySlots"
         [Comp.mkFct md "DelaySlots" args sigName strBody
         ]
   end 
end
