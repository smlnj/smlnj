(* code from Issue 310 *)

structure Main =
struct
  val showSet = fn t0 =>
    "["
    ^ String.concatWith ", " (List.map Int.toString (IntBinarySet.listItems t0))
    ^ "]"

  fun run () =
    let
      val set1 = IntBinarySet.singleton 5356
      val set2 = IntBinarySet.fromList [4986, 5360, 5361]
      val set3 = IntBinarySet.union (set1, set2)
      val set4 = IntBinarySet.union (IntBinarySet.singleton 5358, set3)
      val set5 = IntBinarySet.intersection (IntBinarySet.singleton 5360, set4)
      val set6 = IntBinarySet.difference (set4, IntBinarySet.singleton 5360)
    in
      print ("Set3: " ^ showSet set3 ^ "\n");
      print ("Set4: " ^ showSet set4 ^ "\n");
      print ("Set5: " ^ showSet set5 ^ "\n");
      print ("Set6: " ^ showSet set6 ^ "\n")
    end
end;

(* Main.run should produce

Set3: [4986, 5356, 5360, 5361]
Set4: [4986, 5356, 5358, 5360, 5361]
Set5: [5360]
Set6: [4986, 5356, 5358, 5361]

*)
