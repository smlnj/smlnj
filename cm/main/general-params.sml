(*
 * "General" parameters that may differ from invocation to invocation of
 * CM.  The "info" type bundles them up so they can be passed around
 * more conveniently.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure GeneralParams = struct

    type param = { penv: SrcPath.env,
		   fnpolicy: FilenamePolicy.policy,
		   symval: string -> { get: unit -> int option,
				       set: int option -> unit },
		   archos: string,
		   keep_going: bool,
		   slave_mode: bool }

    type info = { param: param,
		  groupreg: GroupReg.groupreg,
		  errcons: PrettyPrint.device,
		  youngest: TStamp.t ref }

    fun bind { param = { penv, archos, fnpolicy, symval,
			 keep_going, slave_mode },
		groupreg, errcons, youngest } rb =
	{ param = { penv = SrcPath.bind penv rb,
		    archos = archos,
		    fnpolicy = fnpolicy,
		    symval = symval,
		    keep_going = keep_going,
		    slave_mode = slave_mode },
	  groupreg = groupreg, errcons = errcons, youngest = youngest }
end
