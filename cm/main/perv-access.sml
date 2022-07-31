(*
 * Definition of a fake structure symbol used to
 * access the pervasive environment.
 *
 * (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure PervAccess = struct
    val pervStrSym = Symbol.strSymbol "<Pervasive>"
end
