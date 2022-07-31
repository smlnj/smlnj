structure E = struct

    exception E
    val _ = (raise E; ())
end
