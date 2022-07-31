signature GENERATE_FILE =
sig 

    val gen : {trans      : string -> string,
               fileSuffix : string,
               program    : string
              } -> string * string list -> int

end
