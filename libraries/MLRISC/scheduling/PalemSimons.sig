(*
 * This algorithm is in Krishna and Barbara Simons' paper from TOPLAS '93.
 * Note: the dag is assumed to be in transitively closed form.
 * 
 * -- Allen
 *)
signature PALEM_SIMONS =
sig

   val rank : { dag : ('n,'e,'g) Graph.graph, (* dag *)
                l   : 'e Graph.edge -> int,   (* latency *)
                d   : 'n Graph.node -> int,   (* individual deadlines *)
                m   : int                     (* number of processors *)
              } -> 
              { d'  : int Array.array (* modified deadlines *)
              }
end
