(* os-path.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *
 * Win32 implementation of the OS.Path structure.
 *)

local
    structure String = StringImp
in
structure OS_Path = OS_PathFn (
  struct
      structure W32G = Win32_General
      structure C = Char
      structure S = String
      structure SS = Substring

      exception Path

      datatype arc_kind = Null | Parent | Current | Arc of string

      fun classify "" = Null
	| classify "." = Current
	| classify ".." = Parent
	| classify a = Arc a

      val parentArc  = ".."
      val currentArc = "."

      (* / should also be disabled on windows, but it is used
       * within CM, so we need to allow it.
       *)
      val validArc = CharVector.all (Char.notContains "\\:*?\"<>|")

      val volSepChar = #":"

      val arcSepChar = W32G.arcSepChar
      val arcSep = S.str arcSepChar

      fun volPresent vol = 
          (String.size vol >= 2) andalso
	  (C.isAlpha(S.sub(vol,0)) andalso (S.sub(vol,1) = volSepChar))

      fun validVolume (_,vol) = 
	  (SS.isEmpty vol) orelse volPresent(SS.string vol)

      val emptySS    = SS.full ""

      fun splitPath (vol, s) = 
	  if (SS.size s >= 1) andalso (SS.sub(s, 0) = arcSepChar) then
	       (true, vol, SS.triml 1 s)
	  else (false, vol, s)

      fun splitVolPath "" = (false, emptySS, emptySS)
	| splitVolPath s = 
	  if volPresent s then splitPath (SS.splitAt (SS.full s, 2))
	  else splitPath (emptySS, SS.full s)

      fun joinVolPath arg = 
	  let fun checkVol vol = if (volPresent vol) then vol else raise Path
	      fun aux (true,"","") = arcSep
		| aux (true,"",s) = arcSep^s
		| aux (true,vol,"") = (checkVol vol)^arcSep
		| aux (true,vol,s) = (checkVol vol)^arcSep^s
		| aux (false,"",s) = s
		| aux (false,vol,"") = checkVol vol
		| aux (false,vol,s) = (checkVol vol)^s
	  in  aux arg
	  end

      fun sameVol (v1, v2) =
	  (* volume names are case-insensitive *)
	  v1 = v2 orelse
	  String.map Char.toLower v1 = String.map Char.toLower v2
  end);
end
