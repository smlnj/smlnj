(*
 *  How to build the demo using version 110.25
 *  Does not work with later versions because of changes to the new CM.
 *)

CM.autoload "full-cm.cm";
val _ = app CM.Anchor.cancel ["MLRISC.cm","Control.cm","Lib.cm"];

CM.make "../cm/MLRISC.cm";
CM.make "sources.cm";
