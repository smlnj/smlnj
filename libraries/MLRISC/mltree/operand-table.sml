(*
 * A table for storing operands for a compilation unit.
 * We give each distinct operand a unique (negative) value number.
 *)
functor OperandTable(Props : INSN_PROPERTIES) : OPERAND_TABLE =
struct

   structure I  = Props.I
   structure C  = I.C
   structure IH = IntHashTable
   structure H  = HashTable
       
   type valueNumber = C.cell

   datatype const =
     INT     of int           (* small integer operands *)
   | INTINF  of IntInf.int    (* large integer operands *)
   | OPERAND of I.operand     (* other operand *)

   structure IntInfMap =
      RedBlackMapFn(type ord_key = IntInf.int
                    val compare = IntInf.compare
                   )

   datatype operandTable =
      TABLE of 
      {  intTable   : valueNumber IH.hash_table,
         miTable    : valueNumber IntInfMap.map ref,
         opnTable   : (I.operand,valueNumber) H.hash_table,
         nextValueNumber : int ref
      }

   datatype valueNumberMethods =
      VALUE_NUMBERING of
      { int     : int -> valueNumber,
        word    : word -> valueNumber,
        int32   : Int32.int -> valueNumber,
        word32  : Word32.word -> valueNumber,
        intinf  : IntInf.int -> valueNumber,
        operand : I.operand -> valueNumber
      }

   exception NoOperand
   exception NoConst
   exception NoInt
   exception NoIntInf

   val gp = C.cellkindDesc C.GP

   exception CONST of const

   fun mkConst(vn, const) = 
       C.CELL{id=vn, an=ref [CONST const], col=ref C.PSEUDO, desc=gp}

   val bot = C.CELL{id= ~9999999, an=ref [], col=ref C.PSEUDO, desc=gp}
   val top = C.CELL{id= ~9999998, an=ref [], col=ref C.PSEUDO, desc=gp}
   val volatile = C.CELL{id= ~9999997, an=ref [], col=ref C.PSEUDO, desc=gp}

   fun create(nextValueNumber) =
   let 
       val opnTable = H.mkTable(Props.hashOpn,Props.eqOpn) (32,NoOperand)
       val intTable = IH.mkTable (32, NoInt)
       val miTable  = ref IntInfMap.empty

       fun newInt i =
       let val vn = !nextValueNumber (* value number *)
           val _ = nextValueNumber := vn - 1;
           val v = mkConst(vn, INT i)
       in  IH.insert intTable (i, v)
       end

       fun init(n,0) = ()
         | init(n,m) = (newInt n; init(n+1,m-1))

   in  init(0,2);
       TABLE{ intTable        = intTable,
              miTable         = miTable,
              opnTable        = opnTable,
              nextValueNumber = nextValueNumber
            }
   end

   fun wordToIntInf w   = IntInf.fromInt(Word.toIntX w)
   fun word32ToIntInf w = Word32.toLargeIntX w
   fun wordToInt w      = Word.toIntX w
   fun word32ToInt w    = Word32.toIntX w
   fun intInfToInt i    = IntInf.toInt i
   fun intInfToInt32 i  = Int32.fromLarge i
   fun intToIntInf i    = IntInf.fromInt i
   fun intToInt32 i     = Int32.fromInt i
   fun int32ToIntInf i  = Int32.toLarge i
   fun int32ToInt i     = Int32.toInt i
   
   (* Lookup the value number of a constant *)
   fun int(TABLE{intTable, ...}) = IH.lookup intTable  

   fun word(TABLE{intTable, ...}) w = IH.lookup intTable (wordToInt w)

   fun word32(TABLE{intTable, miTable, ...}) w = 
         IH.lookup intTable (word32ToInt w) handle Overflow =>
          case IntInfMap.find(!miTable, word32ToIntInf w) of
             SOME v => v
          |  NONE => raise NoIntInf

   fun int32(TABLE{intTable, miTable, ...}) w = 
         IH.lookup intTable (int32ToInt w) handle Overflow =>
          case IntInfMap.find(!miTable, int32ToIntInf w) of
             SOME v => v
          |  NONE => raise NoIntInf

   fun intinf(TABLE{intTable, miTable, ...}) i = 
         IH.lookup intTable (intInfToInt i) handle Overflow =>
          case IntInfMap.find(!miTable,i) of
            SOME v => v
          | NONE => raise NoIntInf

   fun operand(TABLE{opnTable,...}) = H.lookup opnTable

   fun lookupValueNumbers tbl =
       VALUE_NUMBERING
       { int = int tbl,
         word = word tbl,
         word32 = word32 tbl,
         int32 = int32 tbl,
         intinf = intinf tbl,
         operand = operand tbl
       }

   (* create new value numebers *)
   fun makeNewValueNumbers(TABLE{opnTable,
                                 nextValueNumber,intTable,miTable,...}) =
   let val findOpn = H.find opnTable
       val findInt = IH.find intTable
       val insertOpn = H.insert opnTable
       val insertInt = IH.insert intTable

       fun newConst(const) = 
       let val vn = !nextValueNumber
       in  nextValueNumber := vn - 1;
           mkConst(vn,const)
       end

       fun mkOpn opn = 
           case findOpn opn of
             SOME v => v 
           | NONE => let val v = newConst(OPERAND opn)
                     in  insertOpn(opn, v); v end
       fun mkInt i =
           case findInt i of
             SOME v => v
           | NONE => let val v = newConst(INT i)
                     in  insertInt(i, v); v end

       fun insertIntInf(i, v) =
           miTable := IntInfMap.insert(!miTable, i, v)

       fun mkIntInf' i =
           case IntInfMap.find(!miTable, i) of
             SOME v => v
           | NONE => let val v = newConst(INTINF i)
                     in  insertIntInf(i, v); v end

       fun mkIntInf i = mkInt(intInfToInt i) handle _ => mkIntInf' i

       fun mkWord w = mkInt(wordToInt w)

       fun mkInt32 i = mkInt(int32ToInt i)
                        handle _ => mkIntInf'(int32ToIntInf i)

       fun mkWord32 w = mkInt(word32ToInt w)
                        handle _ => mkIntInf'(word32ToIntInf w)
   in  VALUE_NUMBERING
       {int=mkInt,
        word=mkWord,
        word32=mkWord32,
        int32=mkInt32,
        intinf=mkIntInf,
        operand=mkOpn
       }
   end

   (* value number -> const *)
   fun const(C.CELL{an, ...}) = 
   let fun find(CONST c::_) = c
         | find(_::an) = find an
         | find [] = raise NoConst
   in  find(!an) end

end
