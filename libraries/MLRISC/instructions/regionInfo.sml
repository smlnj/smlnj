signature REGION_INFO =
sig

   (*
    * An action associated with a region:
    * This can be initialization, read, or update
    *
    * Mutability: is the region writable?
    * This can be mutatble or immutable
    * 
    * A strong update to the same exact location always override the 
    * previous action.
    *)
   datatype action = INIT | READ | UPDATE
   type kind = {action:action, strong:bool, mutable:bool}

end

structure RegionInfo =
struct
   datatype action = INIT | READ | UPDATE
   type kind = {action:action, strong:bool, mutable:bool}
end
