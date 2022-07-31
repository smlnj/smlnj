CM.autoload "$smlnj/cmb.cm";
fun serv n =
    CM.Server.start { name = n, pref = 0, pathtrans = NONE,
		      cmd = ("./testml", ["sml", "@CMslave"]) };
