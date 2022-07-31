signature S = sig
    type t
    val x : t
end

structure S :> S = struct
    local
	type u = real
	val y : u = 1.0
    in
	type t = int
	val x = 1
    end
end
