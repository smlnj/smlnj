(* skeleton-file-pickle.sml
 *
 * Generated from skeleton.asdl by asdlgen.
 *)

structure SkeletonFilePickle : SKELETON_PICKLE
  where type instream = ASDLFilePickle.instream
  where type outstream = ASDLFilePickle.outstream = struct
    type instream = ASDLFilePickle.instream
    type outstream = ASDLFilePickle.outstream
    (*---------- begin rd-util.in ----------*)
    (* read an option *)
    fun readOption rdFn inS = (case ASDLFilePickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn inS in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
    (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLFilePickle.readUInt inS
          fun read (0w0, xs) = reverse (xs, [])
            | read (n, xs) = let
                val x = rdFn inS
                in
                  read (n-0w1, x::xs)
                end
          and reverse ([], xs) = xs
            | reverse (x::xr, xs) = reverse (xr, x::xs)
          in
            read (len, [])
          end
(*---------- end rd-util.in ----------*)
(*---------- begin wr-util.in ----------*)
    (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLFilePickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLFilePickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
    (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLFilePickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
(*---------- end wr-util.in ----------*)

    fun read_namespace (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => SkeletonRep.VALspace
            | 0w2 => SkeletonRep.TYCspace
            | 0w3 => SkeletonRep.SIGspace
            | 0w4 => SkeletonRep.STRspace
            | 0w5 => SkeletonRep.FCTspace
            | 0w6 => SkeletonRep.FIXspace
            | 0w7 => SkeletonRep.LABspace
            | 0w8 => SkeletonRep.TYVspace
            | 0w9 => SkeletonRep.FSIGspace
            | _ => raise ASDL.DecodeError)
    fun read_symbol (inS : instream) = let
          val pkl = let
          val x0 = read_namespace inS
          val x1 = ASDLFilePickle.readString inS
          in
            (x0, x1)
          end
          in SkeletonRep.symbolFromPkl pkl
          end
    fun read_symset (inS : instream) = let
          val pkl = readSeq read_symbol inS
          in SkeletonRep.symsetFromPkl pkl
          end
    fun read_sympath (inS : instream) = let
          val pkl = readSeq read_symbol inS
          in SkeletonRep.spathFromPkl pkl
          end
    fun read_modExp (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => let val x0 = read_sympath inS in SkeletonRep.Var (x0) end
            | 0w2 => let val x0 = readSeq read_decl inS in SkeletonRep.Decl (x0) end
            | 0w3 => let
              val x0 = readSeq read_decl inS
              val x1 = read_modExp inS
              in
                SkeletonRep.Let (x0, x1)
              end
            | 0w4 => let
              val x0 = read_modExp inS
              val x1 = read_modExp inS
              in
                SkeletonRep.Ign1 (x0, x1)
              end
            | _ => raise ASDL.DecodeError)
    and read_decl (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => let
              val x0 = read_symbol inS
              val x1 = read_modExp inS
              in
                SkeletonRep.Bind (x0, x1)
              end
            | 0w2 => let
              val x0 = read_decl inS
              val x1 = read_decl inS
              in
                SkeletonRep.Local (x0, x1)
              end
            | 0w3 => let val x0 = readSeq read_decl inS in SkeletonRep.Par (x0) end
            | 0w4 => let val x0 = readSeq read_decl inS in SkeletonRep.Seq (x0) end
            | 0w5 => let val x0 = read_modExp inS in SkeletonRep.Open (x0) end
            | 0w6 => let val x0 = read_symset inS in SkeletonRep.Ref (x0) end
            | _ => raise ASDL.DecodeError)
    fun write_namespace (outS : outstream, obj) = (case obj
           of SkeletonRep.VALspace => ASDLFilePickle.writeTag8 (outS, 0w1)
            | SkeletonRep.TYCspace => ASDLFilePickle.writeTag8 (outS, 0w2)
            | SkeletonRep.SIGspace => ASDLFilePickle.writeTag8 (outS, 0w3)
            | SkeletonRep.STRspace => ASDLFilePickle.writeTag8 (outS, 0w4)
            | SkeletonRep.FCTspace => ASDLFilePickle.writeTag8 (outS, 0w5)
            | SkeletonRep.FIXspace => ASDLFilePickle.writeTag8 (outS, 0w6)
            | SkeletonRep.LABspace => ASDLFilePickle.writeTag8 (outS, 0w7)
            | SkeletonRep.TYVspace => ASDLFilePickle.writeTag8 (outS, 0w8)
            | SkeletonRep.FSIGspace => ASDLFilePickle.writeTag8 (outS, 0w9))
    fun write_symbol (outS : outstream, obj) = let
          val pkl = SkeletonRep.symbolToPkl obj
          val (x0, x1) = pkl
          in
            write_namespace (outS, x0);
            ASDLFilePickle.writeString (outS, x1)
          end
    fun write_symset (outS : outstream, obj) = let
          val pkl = SkeletonRep.symsetToPkl obj
          in writeSeq write_symbol (outS, pkl)
          end
    fun write_sympath (outS : outstream, obj) = let
          val pkl = SkeletonRep.spathToPkl obj
          in writeSeq write_symbol (outS, pkl)
          end
    fun write_modExp (outS : outstream, obj) = (case obj
           of SkeletonRep.Var x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w1);
              write_sympath (outS, x0))
            | SkeletonRep.Decl x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w2);
              writeSeq write_decl (outS, x0))
            | SkeletonRep.Let(x0, x1) => (
              ASDLFilePickle.writeTag8 (outS, 0w3);
              writeSeq write_decl (outS, x0);
              write_modExp (outS, x1))
            | SkeletonRep.Ign1(x0, x1) => (
              ASDLFilePickle.writeTag8 (outS, 0w4);
              write_modExp (outS, x0);
              write_modExp (outS, x1)))
    and write_decl (outS : outstream, obj) = (case obj
           of SkeletonRep.Bind(x0, x1) => (
              ASDLFilePickle.writeTag8 (outS, 0w1);
              write_symbol (outS, x0);
              write_modExp (outS, x1))
            | SkeletonRep.Local(x0, x1) => (
              ASDLFilePickle.writeTag8 (outS, 0w2);
              write_decl (outS, x0);
              write_decl (outS, x1))
            | SkeletonRep.Par x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w3);
              writeSeq write_decl (outS, x0))
            | SkeletonRep.Seq x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w4);
              writeSeq write_decl (outS, x0))
            | SkeletonRep.Open x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w5);
              write_modExp (outS, x0))
            | SkeletonRep.Ref x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w6);
              write_symset (outS, x0)))
  end

