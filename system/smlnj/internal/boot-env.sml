(* boot-env.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Run the BootEnvF functor which builds the boot environments.
 *
 * It is important that this functor is done executing by the time
 * the code for the InteractiveSystem runs.  Otherwise we would never
 * be able to get rid of CM/CMB from an interactive heap image.
 *)

structure BootEnv = BootEnvF (

    datatype envrequest = datatype CM0.envrequest

    val architecture = Backend.architecture

    val cminit = CM0.init

    fun cmbmake (nbd, light) = (
	  if light then #set (CMB.symval "LIGHT") (SOME 1) else ();
          #set (CMB.symval "CMB_REBUILD_MODE") (SOME 1);
          #set (CMB.symval "NO_PLUGINS") (SOME 1);
          ignore (CMB.make' (SOME nbd))))
