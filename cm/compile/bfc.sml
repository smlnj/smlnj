(* bfc.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Keeping binfiles for short periods of time.
 *   This is used in "stabilize" and in "make" where first there is a
 *   "compile" traversal that produces certain binfile contents, and
 *   then there is a "consumer" traversal that uses the binfile contents.
 *   No error checking is done -- the "get" operation assumes that the
 *   stuff is either in its cache or in the file system.
 *   Moreover, the static environment cannot be used (BF.senvOf will fail
 *   if the binfile had to be reloaded from disk).
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

signature BFC = sig
    type bfc
    type stats = { env: int, data: int, code: int }
    val new : unit -> { store: SmlInfo.info * { contents: bfc, stats: stats }
			       -> unit,
		        get: SmlInfo.info -> { contents: bfc, stats: stats } }
    val getStable : { stable: string, offset: int, descr: string } -> bfc
  end

functor BfcFn (val arch: string) :> BFC
    where type bfc = Binfile.bfContents =
  struct

    structure BF = Binfile
    type bfc = BF.bfContents
    type stats = { env: int, data: int, code: int }

    (* version info for binfiles *)
    val version = BF.mkVersion {
            arch = arch,
            smlnjVersion = SMLNJVersion.version'
          }

    fun new () = let
	val m = ref SmlInfoMap.empty
	fun store (i, x) = m := SmlInfoMap.insert (!m, i, x)
	fun get i = (case SmlInfoMap.find (!m, i)
               of SOME x => x
	        | NONE => let
		    val binname = SmlInfo.binname i
		    fun reader s = let
			  val x = BF.read { stream = s, offset = 0, version = version }
		          in
			    store (i, x);
			    x
		          end
		    in
		      SafeIO.perform {
                          openIt = fn () => BinIO.openIn binname,
                          closeIt = BinIO.closeIn,
                          work = reader,
                          cleanup = fn _ => ()
                        }
		    end
              (* end case *))
        in
          { store = store, get = get }
        end

    fun getStable { stable, offset, descr } = let
	  fun work s = let
                val offset = Position.fromInt offset
                in
(*DEBUG*)print(concat[
"# getStable {stable = \"", stable, "\", offset = ", Position.toString offset,
", descr = \"", descr, "\"}\n"]);
                  Seek.seek (s, offset);
                  (* We can use an empty static env because no
                   * unpickling will be done.
                   *)
                  #contents (BF.read { stream = s, offset = offset, version = version })
                end
          in
	    SafeIO.perform {
                openIt = fn () => BinIO.openIn stable,
                closeIt = BinIO.closeIn,
                work = work,
                cleanup = fn _ => ()
              }
          end

  end (* functor BfcFn *)
