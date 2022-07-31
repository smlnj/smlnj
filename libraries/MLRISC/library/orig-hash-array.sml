(*
 * Dynamic (sparse) array that uses hashing
 *
 * -- Allen
 *)

structure HashArray : 
     sig include ARRAY
         val array' : int * (int -> 'a) -> 'a array
         val array'': int * (int -> 'a) -> 'a array
         val clear  : 'a array -> unit 
         val remove : 'a array * int -> unit
         val dom    : 'a array -> int list
         val copy_array : 'a array -> 'a array
     end =
struct
    structure A = Array

    datatype 'a default = V of 'a | F of int -> 'a | U of int -> 'a
    datatype 'a array = 
       ARRAY of (int * 'a) list A.array ref * 'a default * int ref * int ref

    type 'a vector = 'a Vector.vector

    val maxLen   = A.maxLen

    exception HashArrayUnimplemented

    fun unimplemented _ = raise HashArrayUnimplemented

    fun array(n,d) = ARRAY(ref(A.array(16,[])),V d,ref n,ref 0)
    fun array'(n,f) = ARRAY(ref(A.array(16,[])),F f,ref n,ref 0)
    fun array''(n,f) = ARRAY(ref(A.array(16,[])),U f,ref n,ref 0)
    fun clear(ARRAY(r,d,n,c)) = (r := A.array(16,[]); n := 0; c := 0)

    fun roundsize n =
    let fun loop i = if i >= n then i else loop(i+i)
    in  loop 1 end 

    fun copy_array(ARRAY(ref a,d,ref n,ref c)) = 
         let val a' = A.array(n,[])
             val _  = A.copy{src=a,dst=a',si=0,di=0,len=NONE}
         in  ARRAY(ref a',d,ref n,ref c)
         end

    val itow = Word.fromInt
    val wtoi = Word.toIntX
    fun index(a, i) = wtoi(Word.andb(itow i, itow(Array.length a - 1)))

    fun tabulate(n,f) =
    let val N = n*n+1
        val N = if N < 16 then 16 else roundsize N
        val a = A.array(N,[])
        fun ins i = 
            let val pos = index(a, i)
                val x   = f i
            in  A.update(a,pos,(i,x)::A.sub(a,pos)); x
            end
        fun insert 0 = ins 0
          | insert i = (ins i; insert(i-1))
    in  if n < 0 then
          ARRAY(ref a,F(fn _ => raise Subscript),ref 0,ref 0)
        else
          ARRAY(ref a,V(insert(n-1)),ref n,ref n)
    end

    fun fromList l =
    let val n = length l
        val N = n*n+1
        val N = if N < 16 then 16 else roundsize N
        val a = A.array(N,[])
        fun ins(i,x) = 
            let val pos = index(a,i)
            in  A.update(a,pos,(i,x)::A.sub(a,pos)); x
            end
        fun insert(i,[])   = F(fn _ => raise Subscript)
          | insert(i,[x])  = V(ins(i,x))
          | insert(i,x::l) = (ins(i,x); insert(i+1,l))
    in  ARRAY(ref a,insert(0,l),ref n,ref n)
    end

    fun length(ARRAY(_,_,ref n,_)) = n

    fun sub(a' as ARRAY(ref a,d,_,_),i) = 
    let val pos = index(a,i)
        fun search [] = (case d of
                           V d => d
                         | F f => f i
                         | U f => let val x = f i
                                  in  update(a',i,x); x end
                        )
          | search ((j,x)::l) = if i = j then x else search l
    in  search(A.sub(a,pos)) end

    and update(a' as ARRAY(ref a,_,n,s as ref size),i,x) =
    let val N   = A.length a
        val pos = index(a,i)
        fun change([],l) = 
              if size+size >= N then grow(a',i,x)
              else (s := size + 1; A.update(a,pos,(i,x)::l))
          | change((y as (j,_))::l',l) = 
              if j = i then A.update(a,pos,(i,x)::l'@l)
              else change(l',y::l)
    in
        change(A.sub(a,pos),[]);
        if i >= !n then n := i+1 else ()
    end

    and grow(ARRAY(a' as ref a,_,_,_),i,x) = 
    let val N   = A.length a
        val N'  = N+N
        val a'' = A.array(N',[])
        fun insert(i,x) = 
            let val pos = index(a'',i)
            in  A.update(a'',pos,(i,x)::A.sub(a'',pos)) end
    in  
        A.app (List.app insert) a;
        insert(i,x);
        a' := a''
    end

    fun remove(a' as ARRAY(ref a,_,n,s as ref size),i) =
    let val N   = A.length a
        val pos = index(a,i)
        fun change([],_) = ()
          | change((y as (j,_))::l',l) = 
              if j = i then (s := size - 1; A.update(a,pos,l'@l))
              else change(l',y::l)
    in  change(A.sub(a,pos),[])
    end

    fun extract (a as ARRAY(_,_,ref n,_),i,j) = 
    let val j = case j of SOME j => i+j | NONE => n
        fun f(k,l) = if k < i then l else f(k-1,sub(a,k)::l)
    in 
        Vector.fromList(f(j-1,[]))
    end

    fun copy { src = src as ARRAY(_,_,ref n,_), si, len, dst, di } = 
    let val j = case len of SOME len => si+len | NONE => n
        fun f(k,k') = if k >= j then ()
                      else (update(dst,k',sub(src,k)); f(k+1,k'+1))
    in  f(si,di)
    end

    val copyVec = unimplemented

    fun app f (ARRAY(ref a,_,_,_)) = A.app (List.app (fn (_,x) => f x)) a
    fun foldl f e (ARRAY(ref a,_,_,_)) =
       A.foldl (fn (l,e) => List.foldl (fn ((_,x),e) => f(x,e)) e l) e a
    fun foldr f e (ARRAY(ref a,_,_,_)) =
       A.foldr (fn (l,e) => List.foldr (fn ((_,x),e) => f(x,e)) e l) e a

    fun modify f (ARRAY(ref a,_,_,_)) =
       A.modify (List.map (fn (i,x) => (i,f x))) a 

    fun appi f (ARRAY(ref a,_,ref n,_),i,j) =
    let val j = case j of SOME j => i+j | NONE => n
    in  A.app (List.app 
           (fn (k,x) => if k >= i andalso k < j then f(k,x) else ())) a
    end
    fun foldli f e (ARRAY(ref a,_,ref n,_),i,j) = 
    let val j = case j of SOME j => i+j | NONE => n
    in  A.foldl (fn (l,e) => List.foldl 
           (fn ((k,x),e) => if k >= i andalso k < j then f(k,x,e) else e) e l) 
           e a
    end
    fun foldri f e (ARRAY(ref a,_,ref n,_),i,j) = 
    let val j = case j of SOME j => i+j | NONE => n
    in  A.foldr (fn (l,e) => List.foldr 
           (fn ((k,x),e) => if k >= i andalso k < j then f(k,x,e) else e) e l) 
           e a
    end
    fun dom(ARRAY(ref a,_,_,_)) = 
       A.foldl (fn (e,l) => List.foldr (fn ((i,_),l) => i::l) l e) [] a

    fun modifyi f (ARRAY(ref a,_,ref n,_),i,j) =
    let val j = case j of SOME j => i+j | NONE => n
    in  A.modify (List.map(fn (k,x) => if k >= i andalso k < j then (k,f(k,x))
                                       else (k,x))) a
    end
end

