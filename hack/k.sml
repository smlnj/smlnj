signature S =
sig
  type t
end;

signature S1 =
sig
  include S where type t = int
end;

