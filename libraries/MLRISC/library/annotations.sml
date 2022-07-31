(*
 *  User definable annotations.
 *
 *  Note: annotations will now be used extensively in all part of
 *  the optimizer.
 *
 *  Idea is stolen from Stephen Weeks
 * 
 *  -- Allen
 *)

structure Annotations : ANNOTATIONS =
struct

   type annotation = exn
   type annotations = annotation list
   type propList = annotations
   exception NoProperty
   type 'a property = 
         { get      : annotations -> 'a option,
           peek     : annotation -> 'a option,
           lookup   : annotations -> 'a,
           contains : annotations -> bool,
           set      : 'a * annotations -> annotations,
           rmv      : annotations -> annotations,
           create   : 'a -> annotation
         }
   type flag = unit property

   val prettyPrinters = ref [] : (annotation -> string) list ref 

   fun attachPrettyPrinter p = prettyPrinters := p :: !prettyPrinters

   fun toString a =
   let fun pr([]) = ""
         | pr(p::ps) = (p a handle _ => pr ps)
   in  pr(!prettyPrinters) end

   (*
    * Look ma, a real use of generative exceptions!
    *)
   fun 'a new(toString) =
   let exception Annotation of 'a
       fun get [] = NONE
         | get (Annotation x::_) = SOME x
         | get (_::l) = get l
       fun peek(Annotation x) = SOME x
         | peek _ = NONE
       fun lookup [] = raise NoProperty
         | lookup (Annotation x::_) = x
         | lookup (_::l) = lookup l
       fun contains [] = false
         | contains (Annotation _::_) = true
         | contains (_::l) = contains l
       fun set(x,[]) = [Annotation x]
         | set(x,Annotation _::l) = Annotation x::l
         | set(x,y::l) = y::set(x,l)
       fun rmv [] = []
         | rmv (Annotation _::l) = rmv l
         | rmv (x::l) = x::rmv l
   in  case toString of
         NONE   => ()
       | SOME f => attachPrettyPrinter(fn Annotation x => f x | e => raise e);
       { get=get, peek=peek, lookup=lookup, contains=contains,
         set=set, rmv=rmv, create=Annotation
       }
   end

   fun 'a new'{create, toString, get=get'} = 
   let fun get [] = NONE
         | get (x::l) = SOME(get' x) handle _ => get l
       fun peek x = SOME(get' x) handle _ => NONE
       fun lookup [] = raise NoProperty
         | lookup (x::l) = get' x handle _ => lookup l
       fun contains [] = false
         | contains (x::l) = (get' x; true) handle _ => contains l
       fun set(x,[]) = [create x]
         | set(x,a::l) = (get' a; create x::l) handle _ => a::set(x,l)
       fun rmv [] = []
         | rmv (x::l) = (get' x; rmv l) handle _ => x::rmv l
   in  attachPrettyPrinter(toString o get');
       { get=get, peek=peek, lookup=lookup, contains=contains,
         set=set, rmv=rmv, create=create
       }
   end


end

