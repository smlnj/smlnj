signature MLRISC_GLUE =
sig

   structure F : FLOWGRAPH

   val codegen : F.cluster -> F.cluster

end
