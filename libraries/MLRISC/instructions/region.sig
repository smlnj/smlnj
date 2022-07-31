signature REGION = 
sig
   type region
   val stack : region
   val readonly : region
   val memory : region
   val toString : region -> string
end
