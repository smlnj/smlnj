(* BLOCK_NAME
 * 
 * The type name, is used to label basic blocks.
 *)
signature BLOCK_NAMES = sig
  type name
  val default : name
  val toString : name -> string
  val == : name * name -> bool 
end
