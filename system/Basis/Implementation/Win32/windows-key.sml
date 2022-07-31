(* windows-key.sml
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 *
 * Structure with the windows basic flags for registry access levels
 *
 *)

structure Windows_KEY : WINDOWS_KEY =
  struct
    local structure BF = BitFlagsFn ()
    in
      open BF
    end
    
    val allAccess : flags = fromWord(0wxF003F)
    val createLink : flags = fromWord(0wx0020)
    val createSubKey : flags = fromWord(0wx0004)
    val enumerateSubKeys : flags = fromWord(0wx0008)
    val execute : flags = fromWord(0wx20019)
    val notify : flags = fromWord(0wx0010)
    val queryValue : flags = fromWord(0wx0001)
    val read : flags = fromWord(0wx20019)
    val setValue : flags = fromWord(0wx0002)
    val write : flags = fromWord(0wx20006)
  end 


