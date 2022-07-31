
(*
 * Translation from one sort to another
 *)
functor MDLAstConstants(Ast : MDL_AST) : MDL_AST_CONSTANTS = 
struct

   structure Ast = Ast
   structure A   = Ast

   fun ID x = A.IDexp(A.IDENT([],x))

   abstype constTable = TABLE of (A.id * A.exp) list ref * int ref
   with fun newConstTable()  = TABLE(ref [], ref  0)
        fun const(TABLE(entries, counter)) e = 
        let fun lookup [] = 
                let val name = "TMP"^ Int.toString(!counter)
                in  counter := !counter + 1;
                    entries := (name, e) :: !entries;
                    ID name
                end
              | lookup((x,e')::rest) = if e = e' then ID x else lookup rest
        in  lookup(!entries) end
        fun genConsts(TABLE(entries, _)) = 
              map (fn (x,e) => A.VALdecl[A.VALbind(A.IDpat x,e)]) 
                   (rev(!entries))
        fun withConsts f =
        let val tbl    = newConstTable()
            val decl   = f (const tbl)
            val consts = genConsts tbl
        in  case consts of 
               [] => decl
            |  _  => A.LOCALdecl(consts,[decl])
        end
   end 

end
