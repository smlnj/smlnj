(* ip.sml *)

functor F () :> sig end = struct end; 

structure A =
struct

  structure S = F ()

end;

