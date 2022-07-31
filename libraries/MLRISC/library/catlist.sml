(*
 * Constant time concatenable list.  
 *
 * -- Allen
 *)

signature CATNETABLE_LIST =
sig
   type 'a catlist 
   val empty    : 'a catlist
   val null     : 'a catlist -> bool
   val length   : 'a catlist -> int
   val cons     : 'a * 'a catlist -> 'a catlist
   val unit     : 'a -> 'a catlist
   val append   : 'a catlist * 'a catlist -> 'a catlist
   val hd       : 'a catlist -> 'a
   val tl       : 'a catlist -> 'a catlist

   val fromList : 'a list -> 'a catlist
   val toList   : 'a catlist -> 'a list

   val map   : ('a -> 'b) -> 'a catlist -> 'b catlist
   val app   : ('a -> unit) -> 'a catlist -> unit
end

structure CatnetableList :> CATNETABLE_LIST =
struct
    datatype 'a catlist = empty | unit of 'a | @ of 'a catlist * 'a catlist

    fun null empty = true
      | null _     = false

    fun length empty    = 0
      | length (unit _) = 1
      | length (a @ b)  = length a + length b 

    fun hd empty    = raise Empty
      | hd (unit a) = a
      | hd (a @ b)  = hd a

    fun tl empty          = raise Empty
      | tl (unit a)       = empty
      | tl ((unit _) @ a) = a
      | tl ((a @ b) @ c)  = tl(a @ (b @ c))
      | tl (empty @ c)    = tl c

    fun cons(a,empty) = unit a
      | cons(a,b)     = unit a @ b

    fun append(empty,a) = a
      | append(a,empty) = a
      | append(a,b)     = a @ b

    fun map f l = 
    let fun g empty    = empty
          | g (unit a) = unit(f a)
          | g (a @ b)  = (g a) @ (g b)
    in  g l end

    fun app f l =
    let fun g empty    = ()
          | g (unit a) = f a
          | g (a @ b)  = (g a; g b)
    in  g l end

    fun fromList []     = empty
      | fromList (a::b) = cons(a,fromList b)

    fun toList l = 
    let fun g(empty,l)  = l
          | g(unit a,l) = a::l
          | g(a @ b, l) = g(a,g(b,l))
    in  g(l,[]) end

end

