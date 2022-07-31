signature SSA_OPTIMIZER =
sig

   structure F : FLOWGRAPH

   val codegen : F.cluster -> F.cluster

end
