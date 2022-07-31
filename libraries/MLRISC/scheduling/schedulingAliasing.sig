signature SCHEDULING_ALIASING =
sig
   structure Region : REGION

   val write : Region.region -> (int * int) list (* def/use *)
   val read  : Region.region -> int list
end
