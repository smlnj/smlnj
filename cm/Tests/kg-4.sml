(* depends on correct kg-3 but not on faulty kg-1 *)
structure KG4 = struct val x = KG3.x + 1 end
