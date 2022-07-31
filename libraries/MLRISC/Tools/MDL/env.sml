signature ENV =
sig

   type 'a env
   type 'a envir = 'a env ref
   exception Env 
   val env : string -> 'a env
   val envir : string -> 'a envir
   val look : 'a env -> string -> 'a
   val lookup : 'a envir -> string -> 'a
   val look' : 'a env -> 'a -> string -> 'a
   val add  : 'a env -> string * 'a -> 'a env
   val update : 'a envir -> string * 'a -> unit
   val app  : (string * 'a -> unit) -> 'a env -> unit
   val map  : (string * 'a -> 'b) -> 'a env -> 'b list
   val fold : (string * 'a * 'b -> 'b) -> 'b -> 'a env -> 'b
   val union : 'a env * 'a env -> 'a env
   val unions : 'a env list -> 'a env
   val empty : 'a env
   val bind : string * 'a -> 'a env
   val consolidate : 'a env -> 'a env

end

structure Env :> ENV = 
struct

   structure H = HashTable
   datatype 'a env = EMPTY 
                   | TABLE of (string,'a) H.hash_table 
                   | OVERRIDE of 'a env * 'a env
                   | BINDING of string * 'a
   type 'a envir = 'a env ref

   exception Env 

   fun env name = EMPTY
   fun envir name = ref EMPTY
   val empty = EMPTY
   fun look EMPTY _  = raise Env
     | look(BINDING(k,v)) x = if x = k then v else raise Env 
     | look(OVERRIDE(a,b)) x = (look b x handle _ => look a x)
     | look(TABLE t) x = H.lookup t x
   fun look' env default x = look env x handle _ => default
   fun lookup (ref env) x = look env x
   fun union(a,EMPTY) = a
     | union(EMPTY,b) = b
     | union(a,b) = OVERRIDE(a,b)
   fun add env x = union(env,BINDING x)
   fun update env x = env := add (!env) x
   fun flatten env = 
   let val t = H.mkTable (HashString.hashString,op =) (13,Env)
       val add = H.insert t
       fun f EMPTY = ()
         | f(BINDING x) = add x
         | f(OVERRIDE(a,b)) = (f a; f b)
         | f(TABLE t) = H.appi add t
   in  f env; t end
   fun app f env = H.appi f (flatten env)
   fun map f env = List.map f (H.listItemsi (flatten env))
   fun fold f x env = H.foldi f x (flatten env)
   fun unions envs = foldr union EMPTY envs
   fun consolidate env = TABLE(flatten env)
   val bind = BINDING
end
