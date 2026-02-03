(* general-params.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * "General" parameters that may differ from invocation to invocation of
 * CM.  The "info" type bundles them up so they can be passed around
 * more conveniently.
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

structure GeneralParams =
  struct

    type param = {
        (* environment for resolving anchors *)
        penv : SrcPath.env,
        (* file naming conventions *)
        fnpolicy : FilenamePolicy.policy,
        (* get/set operations for CM defines *)
        symval : string -> { get: unit -> int option, set: int option -> unit },
        (* the architecture and operation system string *)
        archos : string,
        (* should CM keep going after an error is encountered? *)
        keep_going : bool,
        slave_mode : bool
      }

    type info = {
        param : param,
        groupreg : GroupReg.groupreg,
        errcons : PrettyPrint.device,
        youngest : TStamp.t ref
      }

    fun bind (info : info) (rb : SrcPath.rebindings) = {
            param = {
                penv = SrcPath.bind (#penv (#param info)) rb,
                archos = #archos (#param info),
                fnpolicy = #fnpolicy (#param info),
                symval = #symval (#param info),
                keep_going = #keep_going (#param info),
                slave_mode = #slave_mode (#param info)
              },
            groupreg = #groupreg info,
            errcons = #errcons info,
            youngest = #youngest info
          }

  end
