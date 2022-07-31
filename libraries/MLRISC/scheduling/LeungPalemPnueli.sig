(*
 * This is the algorithm from PACT '98 (me, Krishna Palem and Amir Pnueli)
 * Note: the dag is assumed to be in transitively closed form.
 *
 * -- Allen
 *)
signature LEUNG_PALEM_PNUELI =
sig

   exception Infeasible

   val rank : { dag : ('n,'e,'g) Graph.graph, (* dag *)
                l   : 'e Graph.edge -> int,   (* latency *)
                r   : 'n Graph.node -> int,   (* individual release times *)
                d   : 'n Graph.node -> int,   (* individual deadlines *)
                m   : int                     (* number of processors *)
              } -> 
              { r' : int Array.array,    (* modified release times *)
                d' : int Array.array     (* modified deadlines *)
              }

end
