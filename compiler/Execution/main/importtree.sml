(* importtree.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure ImportTree = struct
    datatype importTree = ITNODE of (int * importTree) list
    type import = PersStamps.persstamp * importTree
end
