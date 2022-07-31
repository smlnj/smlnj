(*
 * Compile the machine description into an internal digestable form
 *)
functor MDLCompile
   (structure Env       : MDL_ENV
    structure AstPP     : MDL_AST_PRETTY_PRINTER
    structure AstUtil   : MDL_AST_UTIL
    structure Trans     : MDL_AST_TRANSLATION
    structure Consts    : MDL_AST_CONSTANTS
    structure Rewriter  : MDL_AST_REWRITER
    structure TypeUtils : MDL_TYPE_UTILS
      sharing Env.Ast = AstPP.Ast = Trans.Ast = AstUtil.Ast =
              Rewriter.Ast = Consts.Ast = TypeUtils.Ast
   ) : MDL_COMPILE =
struct

   structure Env       = Env
   structure Ast       = Env.Ast
   structure Trans     = Trans
   structure Rewriter  = Rewriter
   structure Consts    = Consts
   structure Error     = MDLError
   structure AstPP     = AstPP
   structure Util      = AstUtil
   structure TypeUtils = TypeUtils
   structure H         = HashTable
      
   open Error Ast Util

   type filename = string

   infix ++
  
   val op ++ = Env.++

   datatype 'a slot = EMPTY of string | SLOT of string * 'a 

   (* machine description *)
   datatype md = MD of 
      {env       : Env.env ref,
       endianess : Ast.endianess slot ref, 
       archKind  : Ast.archKind slot ref,
       asmCase   : Ast.assemblycase slot ref,
       name      : string slot ref,
       filename  : filename, 
       cells     : Ast.storagedecl list ref,
       locations : Ast.locbind list ref,
       formats   : (int option * Ast.formatbind) list ref,
       instructions: Ast.consbind list slot ref,
       debug     : string list ref,
       cpus      : Ast.cpubind list slot ref,
       pipelines : Ast.pipelinebind list slot ref,
       resources : Ast.id list slot ref,
       latencies : Ast.latencybind list slot ref
      }

   fun getSlot(ref(EMPTY name)) = Error.fail(name^" has not been declared") 
     | getSlot(ref(SLOT(_,x))) = x

   fun getSlot'(ref(EMPTY _)) = []
     | getSlot'(ref(SLOT(_,x))) = x

   fun putSlot(s as ref(EMPTY name),x) = s := SLOT(name,x)
     | putSlot(s as ref(SLOT(name,_)),x) =
         Error.error("duplicate declaration of "^name)

   fun putSlot'(s as ref(EMPTY name),x) = s := SLOT(name,x)
     | putSlot'(s as ref(SLOT(name,_)),x) = s := SLOT(name,x)

   (* Extract info from a machine description *)
   fun endianess(MD{endianess, ...}) = getSlot(endianess)
   fun archKind(MD{archKind, ...}) = getSlot(archKind)
   fun asmCase(MD{asmCase, ...}) = getSlot(asmCase)
   fun name(MD{name, ...}) = getSlot(name)
   fun filename(MD{filename, ...}) = filename
   fun env(MD{env, ...}) = !env
   fun cells(MD{cells, ...}) = !cells
   fun locations(MD{locations, ...}) = !locations
   fun formats(MD{formats, ...}) = !formats
   fun instructions(MD{instructions, ...}) = getSlot instructions
   fun debugging(MD{debug, ...}) x = List.exists (fn x' => x = x') (!debug) 
   fun resources(MD{resources, ...}) = getSlot' resources
   fun latencies(MD{latencies, ...}) = getSlot' latencies
   fun cpus(MD{cpus, ...}) = getSlot' cpus
   fun pipelines(MD{pipelines, ...}) = getSlot' pipelines

(*
   fun cellSets(MD{cells, ...}) =
       ListMergeSort.sort
         (fn (CELLdecl{from=f1, ...}, CELLdecl{from=f2, ...}) => !f1 > !f2)
            (List.filter (fn CELLdecl{cellset=true, alias=NONE, ...} => true
                           | CELLdecl _ => false) (!cells)
         )

   fun cellSetsAliases(MD{cells, ...}) = 
        ListMergeSort.sort
         (fn (CELLdecl{from=f1, ...}, CELLdecl{from=f2, ...}) => !f1 > !f2)
           (List.filter (fn CELLdecl{cellset=true, ...} => true
                          | CELLdecl{alias=SOME _, ...} => true
                          | _ => false) (!cells))
 *)

   fun lookupCellKind(MD{cells, ...}) k = 
   let fun loop [] = fail("cellkind "^k^" not found")
         | loop((c as CELLdecl{id, nickname, ...})::cs) =
            if k = id orelse k = nickname then c else loop cs
   in  loop (!cells) end

   fun lookupDatatype(MD{env, ...}) t = 
   let val instrEnv  = Env.lookupStr (!env) (IDENT([],"Instruction"))
       val datatypes = Env.datatypeDefinitions instrEnv
       fun loop [] = fail("datatype "^t^" not found")
         | loop((db as DATATYPEbind{id, ...})::dbs) =
            if t = id then db else loop dbs
   in  loop datatypes end

   fun hasCopyImpl md =
       List.exists(fn CONSbind{id="COPY",ty=SOME(RECORDty fields),...} =>
                      List.exists(fn (id,_) => id = "impl") fields
                    | _ => false
                   ) (instructions md)

   (* Extract info from the environment *)
   val declOf  = Env.declOf o env
   val fctArgOf = Env.fctArgOf o env
   val typeOf = Env.typeOf o env

   (* Require the definitions of these things *)
   fun require md envName {values,types} =
   let val decls = declOf md envName
       val hash = HashString.hashString
       exception NotDefined
       val valueTbl = H.mkTable(hash,op =)(32,NotDefined)
       val typeTbl = H.mkTable(hash,op =) (32,NotDefined)
       fun enterDty(DATATYPEbind{id,...}) = H.insert typeTbl (id,())
         | enterDty(DATATYPEEQbind{id, ...}) = H.insert typeTbl (id,())
       fun enterTy(TYPEbind(id, _, _)) = H.insert typeTbl (id,())
       fun enterFb(FUNbind(id, _)) = H.insert valueTbl (id,())
         
       fun decl _ (d as DATATYPEdecl(dts,ts)) = 
                 (app enterDty dts; app enterTy ts; d)
         | decl _ (d as FUNdecl fbs) = (app enterFb fbs; d)
         | decl _ d = d
       val NO = Rewriter.noRewrite
       val _ = #decl(Rewriter.rewrite{decl=decl, exp=NO, pat=NO,
                                      ty=NO, sexp=NO}) decls
       fun check kind table id = 
            (H.lookup table id)
             handle _ => warning("missing "^kind^" "^envName^"."^id)
   in  app (check "function" valueTbl) values;
       app (check "type" typeTbl) types 
   end

   (* Compile an AST into a machine description *)

   fun compile(filename, decls) = 
   let val endianess   = ref(EMPTY "endianess")
       val archKind    = ref(EMPTY "architecture")
       val asmCase     = ref(EMPTY "assembly case")
       val name        = ref(EMPTY "module name")
       val instructions= ref(EMPTY "instructions")
       val pipelines   = ref(EMPTY "pipelines")
       val resources   = ref(EMPTY "resources")
       val latencies   = ref(EMPTY "latencies")
       val cpus        = ref(EMPTY "cpus")
       val env         = ref Env.empty
       val cells       = ref []
       val locations   = ref []
       val debug       = ref []
       val formats     = ref []
       val md = MD{env      =env,
                   endianess=endianess,
                   archKind =archKind,
                   asmCase  =asmCase,
                   name     =name,
                   filename =filename,
                   cells    =cells,
                   locations=locations,
                   formats  =formats,
                   instructions=instructions,
                   debug    =debug,
                   cpus     =cpus,
                   resources=resources,
                   pipelines=pipelines,
                   latencies=latencies
                  }
       fun decl d = env := ((!env) ++ Env.elab (!env) d)
       fun D d =
           case d of
           (* ML code *)
             DATATYPEdecl _ => decl d
           | FUNdecl _      => decl d
           | VALdecl _      => decl d
           | VALSIGdecl _   => decl d
           | TYPESIGdecl _  => decl d
           | LOCALdecl _    => decl d
           | STRUCTUREdecl _ => decl d
           | INFIXdecl _     => decl d
           | INFIXRdecl _    => decl d
           | NONFIXdecl _    => decl d
           | OPENdecl _      => decl d
           | SEQdecl ds      => Ds ds
           | $ _             => ()
           | MARKdecl(l,d)   => (setLoc l; D d)

           (* MD Gen specific constructions *)
           | FORMATdecl(bits,f) => formats :=  !formats @ 
                                      map (fn f => (bits,f)) f
           | STORAGEdecl d      => cells := !cells @ d 
           | LOCATIONSdecl d    => locations := !locations @ d
           | INSTRUCTIONdecl c  => (putSlot(instructions,c); decl d)
           | ARCHdecl(n,ds)     => (putSlot(name,n); Ds(ds))
           | BITSORDERINGdecl _ => error "bitsordering"
           | ARCHKINDdecl k     => putSlot(archKind, k)
           | ENDIANESSdecl e    => putSlot(endianess, e)
           | NAMEdecl n         => putSlot'(name, n)
           | ASSEMBLYCASEdecl c => putSlot(asmCase, c)
           | DEBUGdecl id       => (debug := id :: !debug)
           | PIPELINEdecl p     => putSlot(pipelines, p)
           | CPUdecl c          => putSlot(cpus, c)
           | RESOURCEdecl r     => putSlot(resources, r)
           | LATENCYdecl l      => putSlot(latencies, l)
           | _ => error "compile"

       and Ds [] = ()
         | Ds (d::ds) = (D d; Ds ds)

   in  Error.init();
       Ds decls;
       md 
   end


   (*
    * Code Generation methods
    *)

   type module = string
   type arguments = string list
   type signatureName = string

   infix ++

   val op ++ = PP.++

   val toupper = String.map Char.toUpper
   val tolower = String.map Char.toLower

   fun signame md suffix = toupper(name md)^suffix
   fun strname md suffix = name md^suffix
   fun fctname md suffix = name md^suffix

   fun mkSigCon "" = PP.nop
     | mkSigCon sign = PP.sp ++ PP.! ":" ++ PP.! sign 

   fun mkSig md name body =
       PP.line(PP.! "signature" ++ PP.! (signame md name) ++ PP.! "=") ++
       PP.line(PP.! "sig") ++
       PP.block(AstPP.decls body) ++
       PP.line(PP.! "end") ++ PP.nl

    fun mkFct' md name args sign body =
       PP.line(PP.! "functor" ++ PP.! (fctname md name) ++
               PP.group("(",")") (AstPP.decl args) ++
               mkSigCon sign ++ PP.! "=") ++
       PP.line(PP.! "struct") ++
       PP.block(AstPP.decls body) ++
       PP.line(PP.! "end") ++ PP.nl

   fun mkFct md name args sign body = mkFct' md name ($ args) sign body

   fun mkStr md name sign body =
       PP.line(PP.! "structure" ++ PP.! (strname md name) ++ 
               mkSigCon sign ++ PP.!"=") ++
       PP.line(PP.! "struct") ++
       PP.block(AstPP.decls body) ++
       PP.line(PP.! "end") ++ PP.nl

   fun mkCode body = PP.block(AstPP.decls body)


   fun pathName md module suffix =
       let fun getName m = 
           OS.Path.concat(OS.Path.dir m,tolower(name md)^OS.Path.file m)
           val pathname = OS.Path.concat(
                        OS.Path.dir(filename md),getName(module^suffix)) 
       in  pathname end
 
   (* Emit text into a file *)
   fun outfile md module suffix text =
   if !errorCount > 0 then () else
   let val file = pathName md module suffix
       (* val file = moduleName(module^".sml") *) (* For testing *)
       val oldText =
           let val stream = TextIO.openIn file
           in  TextIO.inputN(stream,1024*1024) before TextIO.closeIn stream
           end handle _ => ""
       val header =
       "(*\n"^
       " * WARNING: This file was automatically generated by MDLGen (v3.1)\n"^
       " * from the machine description file \""^(filename md)^"\".\n"^
       " * DO NOT EDIT this file directly\n"^
       " *)\n"^
       "\n\n"
       val newText = header^text
   in  if !errorCount = 0 then 
          (print("   Generating module "^file^" ... ");
           if oldText <> newText then
	   let val dir = OS.Path.dir file
	       val _   = OS.FileSys.mkDir dir handle _ => ()
	       val stream = TextIO.openOut file
           in  
	       TextIO.output(stream,newText);
	       TextIO.closeOut stream;
	       print("done\n")
	   end
           else print("file is unchanged\n")
          )
       else ()
   end

   (* Emit code into a file *)
   fun codegen md module code =
   let val newText = PP.text(PP.setmode "code" ++ PP.concat code)
   in  outfile md module ".sml" newText
   end

   fun errorHandler md suffix = ERRORfun(strname md suffix)

   (* Emit a function that dispatches to subfunctions according to the
    * cell kind
    *)
   fun mkQueryByCellKind md name =
   let val cellKinds = cells md
       val clientDefined =  
           List.filter (fn CELLdecl{id, alias, ...} =>
              not(isSome alias) andalso
              not(MLRiscDefs.isPredefinedCellKind id) andalso
              not(MLRiscDefs.isPseudoCellKind id)) cellKinds

       val newlyDefined =
           case clientDefined of
             [] => [CLAUSE([WILDpat],NONE,APP("error",STRINGexp name))]
           | _  => 
              [CLAUSE([IDpat "k"],NONE,
                 foldr(fn (CELLdecl{id, alias, ...}, e) =>
                    IFexp(APP("=",TUPLEexp[ID "k",IDexp(IDENT(["C"],id))]),
                          ID(name^id),
                          e))
                    (APP("error",STRINGexp name)) clientDefined)
              ]

       val predefined =
          foldr (fn (CELLdecl{id, alias, ...}, c) =>
                 if MLRiscDefs.isPredefinedCellKind id andalso 
                    not(MLRiscDefs.isPseudoCellKind id) 
                 then 
                   CLAUSE([CONSpat(IDENT(["C"],id),NONE)],NONE,
                          case alias of 
                            NONE       => ID(name^id)
                          | SOME alias => APP(name,IDexp(IDENT(["C"],alias)))
                         )::c
                 else c
                ) newlyDefined cellKinds 

   in  FUNdecl[FUNbind(name, predefined)]
   end

   (*
    * Do everything on user defined cellkinds
    *)
   fun forallUserCellKinds md f = 
        map f (List.filter (fn CELLdecl{id, alias, ...} =>
               not(MLRiscDefs.isPseudoCellKind id)
               andalso not(isSome alias)
              ) (cells md))


end
