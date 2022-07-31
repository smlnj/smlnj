(* unix-env.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * A UNIX environment is a list of strings of the form "name=value", where
 * the "=" character does not appear in name.
 * NOTE: binding the user's environment as an ML value and then exporting the
 * ML image can result in incorrect behavior, since the environment bound in the
 * heap image may differ from the user's environment when the exported image
 * is used.
 *)

structure UnixEnv : UNIX_ENV =
  struct

    structure SS = Substring

    local
      fun notEqual #"=" = false | notEqual _ = true
      val split = SS.splitl notEqual
    in
    fun splitBinding s = let
	  val (a, b) = split(SS.full s)
	  in
	    if SS.isEmpty b
	      then (s, "")
	      else (SS.string a, SS.string(SS.triml 1 b))
	  end
    end

  (* return the value, if any, bound to the name. *)
    fun getFromEnv (name, env) = let
	  fun look [] = NONE
	    | look (s::r) = let
		val (n, v) = splitBinding s
		in
		  if (n = name) then (SOME v) else look r
		end
	  in
	    look env
	  end

  (* return the value bound to the name, or a default value *)
    fun getValue {name, default, env} = (case getFromEnv(name, env)
	   of (SOME s) => s
	    | NONE => default
	  (* end case *))

  (* remove a binding from an environment *)
    fun removeFromEnv (name, env) = let
	  fun look [] = []
	    | look (s::r) = let
		val (n, v) = splitBinding s
		in
		  if (n = name) then r else (s :: look r)
		end
	  in
	    look env
	  end

  (* add a binding to an environment, replacing an existing binding
   * if necessary.
   *)
    fun addToEnv (nameValue, env) = let
	  val (name, _) = splitBinding nameValue
	  fun look [] = [nameValue]
	    | look (s::r) = let
		val (n, v) = splitBinding s
		in
		  if (n = name) then r else (s :: look r)
		end
	  in
	    look env
	  end

  (* return the user's environment *)
    val environ = Posix.ProcEnv.environ

  (* return the binding of an environment variable in the
   * user's environment.
   *)
    fun getEnv name = getFromEnv(name, environ())

  end; (* UnixEnv *)

