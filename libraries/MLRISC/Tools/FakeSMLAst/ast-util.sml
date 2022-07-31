functor MDLAstUtil(Ast : MDL_AST) : MDL_AST_UTIL =
struct

   structure Ast = Ast
   open Ast

   fun ID id = IDexp(IDENT([],id))
   fun APP(f,e) = APPexp(ID f,e)
   fun BINOPexp(f,x,y) = APP(f,TUPLEexp[x,y])
   fun PLUS(a,LITexp(INTlit 0)) = a
     | PLUS(a,LITexp(WORDlit 0w0)) = a
     | PLUS(a,LITexp(WORD32lit 0w0)) = a
     | PLUS(LITexp(INTlit 0),a) = a
     | PLUS(LITexp(WORDlit 0w0),a) = a
     | PLUS(LITexp(WORD32lit 0w0),a) = a
     | PLUS(a,b) = BINOPexp("+",a,b)
   fun MINUS(a,LITexp(INTlit 0)) = a
     | MINUS(a,LITexp(WORDlit 0w0)) = a
     | MINUS(a,LITexp(WORD32lit 0w0)) = a
     | MINUS(a,b) = BINOPexp("-",a,b)
   fun ANDB(a,b) = BINOPexp("&&",a,b)
   fun ORB(a,b) = BINOPexp("||",a,b)
   fun SLL(a,LITexp(WORDlit 0w0)) = a
     | SLL(a,LITexp(WORD32lit 0w0)) = a
     | SLL(a,LITexp(INTlit 0)) = a
     | SLL(a,b) = BINOPexp("<<",a,b)
   fun SLR(a,LITexp(WORDlit 0w0)) = a
     | SLR(a,LITexp(WORD32lit 0w0)) = a
     | SLR(a,LITexp(INTlit 0)) = a
     | SLR(a,b) = BINOPexp(">>",a,b)
   fun SAR(a,LITexp(WORDlit 0w0)) = a
     | SAR(a,LITexp(WORD32lit 0w0)) = a
     | SAR(a,LITexp(INTlit 0)) = a
     | SAR(a,b) = BINOPexp("~>>",a,b)

   fun BOOLexp x = LITexp(BOOLlit x)
   fun STRINGexp s = LITexp(STRINGlit s)
   fun INTexp x = LITexp(INTlit x)
   fun INT32exp x = LITexp(INT32lit x)
   fun INTINFexp x = LITexp(INTINFlit x)
   fun CHARexp x = LITexp(CHARlit x)
   fun WORDexp x = LITexp(WORDlit x)
   fun WORD32exp x = LITexp(WORD32lit x)

   fun BOOLpat x = LITpat(BOOLlit x)
   fun STRINGpat s = LITpat(STRINGlit s)
   fun INTpat x = LITpat(INTlit x)
   fun INT32pat x = LITpat(INT32lit x)
   fun INTINFpat x = LITpat(INTINFlit x)
   fun CHARpat x = LITpat(CHARlit x)
   fun WORDpat x = LITpat(WORDlit x)
   fun WORD32pat x = LITpat(WORD32lit x)

   val UNIT = TUPLEexp []
   val TRUE = BOOLexp true
   val FALSE = BOOLexp false
   fun ANDALSO(LITexp(BOOLlit true),x) = x
     | ANDALSO(LITexp(BOOLlit false),x) = FALSE
     | ANDALSO(x,LITexp(BOOLlit true)) = x
     | ANDALSO(x,LITexp(BOOLlit false)) = FALSE
     | ANDALSO(x,y) = BINOPexp("andalso",x,y)
   fun ORELSE(LITexp(BOOLlit true),x) = TRUE
     | ORELSE(LITexp(BOOLlit false),x) = x
     | ORELSE(x,LITexp(BOOLlit true)) = TRUE
     | ORELSE(x,LITexp(BOOLlit false)) = x
     | ORELSE(x,y) = BINOPexp("orelse",x,y)
   val NILexp = LISTexp([],NONE)


   val UNITty = IDty(IDENT([],"unit"))
   val BOOLty = IDty(IDENT([],"bool"))
   val INTty = IDty(IDENT([],"int"))
   val REGISTERty = IDty(IDENT([],"CellsBasis.cell"))
   val REGISTERLISTty = APPty(IDENT([],"list"),[REGISTERty])
   val INTLISTty = APPty(IDENT([],"list"),[INTty])
   val STRINGty = IDty(IDENT([],"string"))
   val WORD32ty = IDty(IDENT(["Word32"],"word"))
   val WORDty = IDty(IDENT(["Word"],"word"))
   val LABELty = IDty(IDENT(["Label"],"label"))
   val LABEXPty = IDty(IDENT(["LabelExp"],"labexp"))
   val CONSTty = IDty(IDENT(["Constant"],"const"))
   val CELLKINDty = IDty(IDENT([],"CellsBasis.cellkind"))
   val CELLSETty = IDty(IDENT([],"cellset"))

   fun DATATYPE(id,args,cbs) = 
        DATATYPEbind{id=id,tyvars=args,mc=NONE,asm=false,field=NONE,cbs=cbs}
   fun CONS(id,arg) = CONSbind{id=id,ty=arg,mc=NONE,asm=NONE,rtl=NONE,
                               nop=FLAGoff,nullified=FLAGoff,
                               delayslot=NONE,
                               delaycand=NONE,sdi=NONE,latency=NONE,
                               pipeline=NONE, loc=SourceMapping.dummyLoc}
   fun VAL(id,e) = VALdecl[VALbind(case id of "_" => WILDpat | _ => IDpat id,e)]
   fun FUN'(id,p,e) = FUNbind(id,[CLAUSE([p],NONE,e)])
   fun FUN(id,p,e) = FUNdecl [FUN'(id,p,e)]
   fun LET([],e) = e 
     | LET(d,e) = LETexp(d,[e])


   fun ERROR text = CLAUSE([WILDpat],NONE,APP("error",STRINGexp text))
   fun ERRORfun name = 
       $["fun error msg = MLRiscErrorMsg.error(\""^name^"\",msg)"]
   fun DUMMYfun name = 
       $["fun "^name^" _ = error \""^name^"\""]
 

   fun BITSLICE(e,ranges) =
   let val temp = ID "temp"
       fun gen(tmp, [], pos, e) = e
         | gen(tmp, (a,b)::slices, pos, e) =
           let val width = b - a + 1
               val mask  = Word32.<<(0w1, Word.fromInt width) - 0w1
               val field = SLL(tmp, WORD32exp(Word32.fromInt a))
               val field = ANDB(field, WORD32exp mask)
           in  gen(tmp, slices, pos+width,
                   PLUS(SLL(field, WORD32exp(Word32.fromInt pos)),e))
           end
       fun emit(tmp) = gen(tmp, rev ranges, 0, WORD32exp 0w0)
   in  case ranges of
         [_] => emit e
       | _   => LETexp([VALdecl[VALbind(IDpat "temp",e)]], [emit(ID "temp")])
   end

      (* Add an entry *)
   fun cons(x,LISTexp(a,b)) = LISTexp(x::a,b)
     | cons(x,y)            = LISTexp([x],SOME y)

   (* Append an entry *)
   fun append(x,LISTexp([],NONE)) = x
     | append(x,y) = APP("@",TUPLEexp[x,y])

   fun compareLiteral(x,y) =
   let fun kind(INTlit _) = 0
         | kind(BOOLlit _) = 1
         | kind(STRINGlit _) = 2
         | kind(CHARlit _) = 3
         | kind(WORDlit _) = 4
         | kind(WORD32lit _) = 5
         | kind(INTINFlit _) = 6
         | kind(REALlit _) = 7
         | kind(INT32lit _) = 8
   in  case (x, y) of
         (INTlit x,INTlit y) => Int.compare(x,y)
       | (INT32lit x,INT32lit y) => Int32.compare(x,y)
       | (BOOLlit x,BOOLlit y) => if x = y then EQUAL 
                                  else if x = false then LESS else GREATER
       | (STRINGlit x,STRINGlit y) => String.compare(x,y)
       | (CHARlit x,CHARlit y) => Char.compare(x,y)
       | (WORDlit x,WORDlit y) => Word.compare(x,y)
       | (WORD32lit x,WORD32lit y) => Word32.compare(x,y)
       | (INTINFlit x,INTINFlit y) => IntInf.compare(x,y)
       | (REALlit x,REALlit y) => String.compare(x,y)
       | (x, y) => Int.compare(kind x,kind y)
   end
end
