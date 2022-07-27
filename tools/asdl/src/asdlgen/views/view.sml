(* view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure View : sig

    type t

    datatype entity
      = File
      | Module of AST.ModuleId.t
      | Type of AST.TypeId.t
      | Constr of AST.ConstrId.t

    structure Prop : sig
	type t

	datatype desc = Desc of {
	    name : Atom.atom,		(* the property's name *)
	    accumulator : bool		(* true if values are cumulative *)
	  }

      (* return the property's name *)
        val nameOf : t -> string

      (* has the property instance been defined? *)
	val isDefined : t -> bool

      (* does the property allow multiple definitions? *)
	val isAccumulator : t -> bool

      (* set the property's value; will extend the value if the property is an accumulator *)
	val setValue : t * string -> unit

      end

  (* return the view's name *)
    val nameOf : t -> string

  (* does the view have the given name *)
    val isView : Atom.atom -> t -> bool

  (* find the property instance for the given view, entity, and property name *)
    val findProp : t * entity * Atom.atom -> Prop.t option

  (* `getValue name (view, entity, default)` gets the value for the property `name`
   * associated with `entity` in the `view`.  It returns `[default]` if there is
   * no value set.
   *)
    val getValue : Atom.atom -> t * entity * string -> string list

    val getOptValue : Atom.atom -> t * entity -> string list option

    val getValues : Atom.atom -> t * entity -> string list

    val getBoolValue : Atom.atom -> t * entity -> bool option
    val getBoolValue' : Atom.atom -> t * entity -> bool	(* no property maps to false *)

    val getNames : Atom.atom -> t * entity -> string list

  (* a view template describes the properties that the various entities of a module
   * might have.
   *)
    type template = {
	fileProps : Prop.desc list,
	moduleProps : Prop.desc list,
	typeProps : Prop.desc list,
	consProps : Prop.desc list
      }

  (* create a new view with the given name and properties *)
    val new : string * template -> t

  (* dump the view to stdOut for debugging purposes *)
    val dump : t -> unit

  end = struct

    structure SS = Substring
    structure ATbl = AtomTable

    datatype entity
      = File
      | Module of AST.ModuleId.t
      | Type of AST.TypeId.t
      | Constr of AST.ConstrId.t

  (* hash table on entities *)
    structure ETbl = HashTableFn (
      struct
	type hash_key = entity
	fun hashVal File = 0w17
          | hashVal (Module id) = Word.<<(AST.ModuleId.hash id, 0w2) + 0w1
	  | hashVal (Type id) = Word.<<(AST.TypeId.hash id, 0w2) + 0w2
	  | hashVal (Constr id) = Word.<<(AST.ConstrId.hash id, 0w2) + 0w3
	fun sameKey (File, File) = true
          | sameKey (Module id1, Module id2) = AST.ModuleId.same(id1, id2)
	  | sameKey (Type id1, Type id2) = AST.TypeId.same(id1, id2)
	  | sameKey (Constr id1, Constr id2) = AST.ConstrId.same(id1, id2)
	  | sameKey _ = false
      end)

    datatype prop = Prop of {
	name : Atom.atom,
	entity : entity,
	accumulator : bool,		(* true if values are cumulative *)
	value : string list ref
      }

    structure Prop =
      struct
	type t = prop

	datatype desc = Desc of {
	    name : Atom.atom,		(* the property's name *)
	    accumulator : bool		(* true if values are cumulative *)
	  }

	fun nameOf (Prop{name, ...}) = Atom.toString name

	fun isDefined (Prop{value, ...}) = not(List.null(!value))

	fun isAccumulator (Prop{accumulator, ...}) = accumulator

	fun setValue (Prop{value, accumulator, ...}, v) =
	      if accumulator
		then value := !value @ [v]
		else value := [v]

	fun getValue (Prop{value, ...}) = !value

      (* create a new property instance for an entity *)
	fun new (Desc{name, accumulator}, entity) = Prop{
		name = name,
		entity = entity,
		accumulator = accumulator,
		value = ref[]
	      }
      end

  (* table to map entities to their property tables *)
    type entity_tbl = Prop.t ATbl.hash_table ETbl.hash_table

  (* a view template describes the properties that the various entities of a module
   * might have.
   *)
    type template = {
	fileProps : Prop.desc list,
	moduleProps : Prop.desc list,
	typeProps : Prop.desc list,
	consProps : Prop.desc list
      }

    datatype t = View of {
	name : string,		(* the view's name *)
	template : {		(* template that specifies which properties are supported by
				 * the view *)
	    fileProps : Prop.desc ATbl.hash_table,
	    moduleProps : Prop.desc ATbl.hash_table,
	    typeProps : Prop.desc ATbl.hash_table,
	    consProps : Prop.desc ATbl.hash_table
	  },
	eTbl : entity_tbl	(* table to map entities to their property tables *)
      }

  (* create a new view *)
    fun new (name, tmp : template) = let
	  fun mkTbl pds = let
		val tbl = ATbl.mkTable(16, Fail "template table")
		fun insert (pd as Prop.Desc{name, ...}) = ATbl.insert tbl (name, pd)
		in
		  List.app insert pds;
		  tbl
		end
	  in
	    View{
		name = CharVector.map Char.toLower name,
		template = {
		    fileProps = mkTbl (#fileProps tmp),
		    moduleProps = mkTbl (#moduleProps tmp),
		    typeProps = mkTbl (#typeProps tmp),
		    consProps = mkTbl (#consProps tmp)
		  },
		eTbl = ETbl.mkTable (64, Fail "view-entity table")
	      }
	  end

  (* test if name' is the name of the view; this test is case insensitive
   * (we assume that the official names of views are lower-case).
   *)
    fun isView name' = let
	  val name' = CharVector.map Char.toLower (Atom.toString name')
          in
	    fn (View{name, ...}) => (name = name')
	  end

    fun nameOf (View{name, ...}) = name

  (* find a property instance; we create a new instance from the template if necessary *)
    fun findProp (View{eTbl, template, ...}, entity, name) = let
	(* extract the appropriate prop descriptor table for the entity *)
	  fun propDescTable () = (case entity
		 of File => #fileProps template
		  | Module _ => #moduleProps template
		  | Type _ => #typeProps template
		  | Constr _ => #consProps template
		(* end case *))
	  in
	    case ETbl.find eTbl entity
	     of SOME pTbl => (case ATbl.find pTbl name
		   of NONE => (case ATbl.find (propDescTable ()) name
			 of SOME pdesc => let
			    (* create the property instance for the entity *)
			      val prop = Prop.new (pdesc, entity)
			      in
				ATbl.insert pTbl (name, prop); (* record property instance *)
				SOME prop
			      end
			  | NONE => NONE (* unknown property *)
			(* end case *))
		    | someProp => someProp
		  (* end case *))
	      | NONE => (case ATbl.find (propDescTable ()) name
		   of SOME pdesc => let
		      (* create the property instance for the entity *)
			val prop = Prop.new (pdesc, entity)
		      (* create new property table for the entity *)
			val pTbl = ATbl.mkTable(16, Fail "prop table")
			in
			  ETbl.insert eTbl (entity, pTbl); (* record property table *)
			  ATbl.insert pTbl (name, prop); (* record property instance *)
			  SOME prop
			end
		    | NONE => NONE
		  (* end case *))
	    (* end case *)
	  end

    fun getValue name (View{eTbl, ...}, entity, default) = (
	  case ETbl.find eTbl entity
	     of SOME pTbl => (case ATbl.find pTbl name
		   of SOME(Prop{value=ref[], ...}) => [default]
		    | SOME(Prop{value=ref s, ...}) => s
		    | NONE => [default]
		  (* end case *))
	      | NONE => [default]
	  (* end case *))

    fun getOptValue name (View{eTbl, ...}, entity) = (
	  case ETbl.find eTbl entity
	     of SOME pTbl => (case ATbl.find pTbl name
		   of SOME(Prop{value=ref s, ...}) => SOME s
		    | NONE => NONE
		  (* end case *))
	      | NONE => NONE
	  (* end case *))

    fun getValues name (View{eTbl, ...}, entity) = (
	  case ETbl.find eTbl entity
	     of SOME pTbl => (case ATbl.find pTbl name
		   of SOME(Prop{value=ref s, ...}) => s
		    | NONE => []
		  (* end case *))
	      | NONE => []
	  (* end case *))

    fun getBoolValue name arg = (case getOptValue name arg
	   of SOME["false"] => SOME false
	    | SOME["true"] => SOME true
	    | _ => NONE
	  (* end case *))

    fun getBoolValue' name arg = (case getOptValue name arg
	   of SOME["false"] => false
	    | SOME["true"] => true
	    | _ => false
	  (* end case *))

    fun getNames name arg = (case getOptValue name arg
	   of SOME[s] =>  List.map
		(fn ss => SS.string (SS.dropl Char.isSpace (SS.dropr Char.isSpace ss)))
		  (SS.fields (fn #"," => true | _ => false) (SS.full s))
	    | _ => []
	  (* end case *))

    fun dump (View{name, template, eTbl}) = let
	  fun pr l = print(concat l)
	  fun prPropDesc (Prop.Desc{name, accumulator}) = pr [
		  "      ", Atom.toString name,
		  if accumulator then " (acc)\n" else "\n"
		]
	  in
	    pr ["***** View ", name, " *****\n"];
	    pr ["** Template:\n"];
	    pr ["    File Properties:\n"];
	    ATbl.app prPropDesc (#fileProps template);
	    pr ["    Module Properties:\n"];
	    ATbl.app prPropDesc (#moduleProps template);
	    pr ["    Type Properties:\n"];
	    ATbl.app prPropDesc (#typeProps template);
	    pr ["    Cons Properties:\n"];
	    ATbl.app prPropDesc (#consProps template);
	    pr ["*****\n"]
	  end

  end
