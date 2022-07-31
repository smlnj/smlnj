CM.autoload "$smlnj/cmb.cm";
fun rserv p n m =
    CM.Server.start { name = n, pref = p, pathtrans = NONE,
	 	      cmd = ("/usr/bin/rsh",
			     [m, "/home/blume/ML/smlnj/bin/sml",
			      "@CMslave"]) };

