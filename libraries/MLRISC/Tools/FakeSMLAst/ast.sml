structure MDLAst : MDL_AST =
struct

   type loc  = SourceMapping.location

   datatype decl = 
     DATATYPEdecl of datatypebind list * typebind list
   | EXCEPTIONdecl of exceptionbind list
   | FUNdecl of funbind list
   | RTLdecl of pat * exp * loc
   | RTLSIGdecl of id list * ty
   | VALdecl of valbind list
   | VALSIGdecl of id list * ty 
   | TYPESIGdecl of id * tyvar list
   | LOCALdecl of decl list * decl list
   | SEQdecl of decl list 
   | STRUCTUREdecl of id * decl list * sigconstraint option * structexp
   | FUNCTORdecl of id * decl list * sigconstraint option * structexp
   | STRUCTURESIGdecl of id * sigexp
   | SIGNATUREdecl of id * sigexp
   | SHARINGdecl of share list
   | OPENdecl of ident list
   | FUNCTORARGdecl of id * sigconstraint
   | INCLUDESIGdecl of sigexp
   | INFIXdecl of int * id list
   | INFIXRdecl of int * id list
   | NONFIXdecl of id list
   | MARKdecl of loc * decl

     (* MD extensions *)
   | ARCHdecl of id * decl list            (* architecture spec *)
   | $ of string list                      (* verbatim code *)
   | BITSORDERINGdecl of range             (* declare bits ordering *)
   | FORMATdecl of int option * formatbind list   
                                           (* declare instruction formats *)
   | ARCHKINDdecl of archKind              (* superscalar/vliw *)
   | ENDIANESSdecl of endianess            (* little/big endian *)
   | STORAGEdecl of storagedecl list       (* cell declarations *) 
   | LOCATIONSdecl of locbind list         (* location declarations *)
   | NAMEdecl of string                    (* name of architecture *)
   | VERSIONdecl of string                 (* version number *)
   | ASSEMBLYCASEdecl of assemblycase      (* lower/uppercase *)
   | INSTRUCTIONdecl of consbind list      (* instruction definition *)
   | DEBUGdecl of id                       (* turn on debugging *)
   | RESOURCEdecl of id list               (* resource declaration *)
   | CPUdecl of cpubind list               (* cpu declaration *)
   | PIPELINEdecl of pipelinebind list     (* pipeline declaration *)
   | LATENCYdecl of latencybind list       (* latency declaration *)

   and   sigexp  = IDsig of ident
                 | WHEREsig of sigexp * ident * structexp
                 | WHERETYPEsig of sigexp * ident * ty
		 | DECLsig of decl list

   and   share   = TYPEshare of ident list
                 | STRUCTshare of ident list 

   and   literal = WORDlit of word
                 | WORD32lit of Word32.word
                 | INTlit of int
                 | INT32lit of Int32.int
                 | INTINFlit of IntInf.int
                 | STRINGlit of string
                 | CHARlit of char
                 | BOOLlit of bool
                 | REALlit of string
   
   and      exp  = LITexp of literal
                 | IDexp of ident
                 | CONSexp of ident * exp option
                 | LISTexp of exp list * exp option
                 | TUPLEexp of exp list
                 | VECTORexp of exp list
                 | RECORDexp of (id * exp) list
                 | APPexp of exp * exp
                 | IFexp of exp * exp * exp
                 | LETexp of decl list * exp list
                 | SEQexp of exp list
                 | RAISEexp of exp 
                 | HANDLEexp of exp * clause list
                 | CASEexp of exp * clause list
                 | TYPEDexp of exp * ty
                 | LAMBDAexp of clause list
                 | MARKexp of loc * exp

                   (* MDL extensions *)
                 | BITSLICEexp of exp * range list
                 | LOCexp of id * exp * id option
                 | ASMexp of assembly
                 | TYPEexp of ty
                 | RTLexp of rtl
                 | CONTexp of exp * id

   and ety       = I8 | I16 | I32 | I64 | FSINGLE | FDOUBLE

   and assemblycase = LOWERCASE | UPPERCASE | VERBATIM

   and structexp = IDsexp of ident
                 | APPsexp of structexp * structexp
                 | DECLsexp of decl list
                 | CONSTRAINEDsexp of structexp * sigexp

   and  ty    = IDty of ident
              | TYVARty of tyvar
              | INTVARty of int
              | VARty of tvkind * int * int ref * ty option ref
              | APPty of ident * ty list
              | FUNty of ty * ty
              | TUPLEty of ty list
              | RECORDty of (id * ty) list
              | POLYty of ty list * ty
              | LAMBDAty of ty list * ty

              | CELLty of id

   and tvkind = INTkind | TYPEkind

   and        pat   = IDpat of id
                    | CONSpat of ident * pat option
                    | WILDpat
                    | ASpat of id * pat
                    | LITpat of literal
                    | LISTpat of pat list * pat option
                    | TUPLEpat of pat list
                    | VECTORpat of pat list
                    | RECORDpat of (id * pat) list * bool
                    | TYPEDpat of pat * ty
                    | NOTpat of pat
                    | ORpat of pat list
                    | ANDpat of pat list
                    | WHEREpat of pat * exp 
                    | NESTEDpat of pat * exp * pat

   and  ident = IDENT of id list * id 

   and  clause = CLAUSE of pat list * guard * exp
  
   and  funbind = FUNbind of id * clause list
   
   and  storagedecl = 
          CELLdecl of {id:id, nickname:id,
                       from:int ref,to:int ref,
                       alias:id option,
                       count:int option,bits:int,print:exp,
                       aggregable:bool,
                       defaults:(int*exp) list}

   and  locbind = LOCbind of id * pat option * exp
   
   and         endianess = LITTLE | BIG
   
   and         archKind = VLIW | SUPERSCALAR
 
   and   formatbind = FORMATbind of id * field list * exp option

   and   field = FIELD of {id:id,width:width,sign:signedness,
                           cnv:cnv, value:Word32.word option}

   and          width = WIDTH of int | RANGE of int * int

   and   cnv = NOcnv
             | CELLcnv of id
             | FUNcnv of id

   and   datatypebind = DATATYPEbind of 
             {id:id,tyvars:tyvar list,mc : opcodeencoding,asm : bool,
              field:id option,cbs:consbind list}
                      | DATATYPEEQbind of {id:id, tyvars:tyvar list, ty:ty}

   and   exceptionbind = EXCEPTIONbind of id * ty option
                       | EXCEPTIONEQbind of id * ident

   and   consbind     = 
         CONSbind of {id : id,ty:ty option,mc : mc option,
                      asm : assembly option,
                      rtl : exp option,
                      nop : flag,
                      nullified : flag,
                      delayslot : exp option,
                      delaycand : exp option,
                      sdi : exp option,
                      latency : exp option,   
                      pipeline : exp option,   
                      loc : loc
                     }

   and   flag      = FLAGon | FLAGoff | FLAGid of id * bool * exp

   and   delayslot = DELAY_ERROR
                   | DELAY_NONE
                   | DELAY_ALWAYS
                   | DELAY_TAKEN
                   | DELAY_NONTAKEN
                   | DELAY_IF of branching * delayslot * delayslot

   and   branching = BRANCHforwards
                   | BRANCHbackwards

   and   mc        = WORDmc of Word32.word
                   | EXPmc of exp

   and   assembly  = STRINGasm of string
                   | ASMasm of asm list

   and   asm       = TEXTasm of string
                   | EXPasm of exp 

   and   typebind  = TYPEbind of id * tyvar list * ty

   and   valbind   = VALbind of pat * exp

   and   signedness   = SIGNED | UNSIGNED

   and   tyvar       = VARtv of id
                     | INTtv of id

   and   rtlterm     = LITrtl of id
                     | IDrtl  of id
                     | COMPOSITErtl of id

   and   cpubind    = CPUbind of {name:id, aliases:string list, maxIssues:int, 
                                  resources: (int * id) list}
   and   pipelinebind = PIPELINEbind of id * (pat * pipelineexp) list

   and   latencybind  = LATENCYbind of id * (pat * exp) list

   and   pipelineexp  = PIPELINE of cycle list

   and   cycle        = ORcycle of cycle * cycle
                      | REPEATcycle of cycle * int
                      | IDcycle of id 


   withtype range = int * int
   and      id    = string
   and      guard = exp option
   and      opcodeencoding = int list option 
   and      cellset = bool
   and      rtl     = rtlterm list
   and      sigconstraint = {abstract: bool, sigexp:sigexp}

end  
