(* test-decode.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TestDecode : sig

    val run : unit -> unit

  end = struct

    structure JU = JSONUtil
    structure JD = JSONDecode

    datatype value = datatype JSON.value
    datatype 'a result = Ok of 'a | Err of exn

    fun pr l = print(concat l)

    fun decode decoder jv = (Ok(JD.decode decoder jv) handle ex => Err ex)

    (* pipe operator for sequencing decode operations *)
    fun |> (x, f) = f x
    infix |>

    (* recursive structure example from Elm *)
    datatype comment = Comment of {
        message : string,
        responses : comment list
      }

    (* test values *)
    val jv00 = NULL
    val jv01 = BOOL false
    val jv02 = BOOL true
    val jv03 = INT 42
    val jv04 = INTLIT "42"
    val jv05 = FLOAT 3.14
    val jv06 = STRING "hello world"
    val jv07 = ARRAY[]
    val jv08 = ARRAY[BOOL false]
    val jv09 = ARRAY[INT 0, INT 1, INT 2]
    val jv10 = OBJECT[]
    val jv11 = OBJECT[("fst", INT 1), ("snd", INT 2)]
    val jv12 = OBJECT[("name", STRING "Jane"), ("age", INT 42)]
    val rv12 = {name = "Jane", age = 42}
    val jv13 = OBJECT[("name", STRING "Bob"), ("age", INT 17)]
    val rv13 = {name = "Bob", age = 17}
    val jv14 = ARRAY[jv12, jv13]
    val jv15 = OBJECT[
          ("message", STRING "hello"),
          ("responses", ARRAY[
              OBJECT[
                  ("message", STRING "goodbye"),
                  ("responses", ARRAY[])
                ]
            ])
        ]
    val rv15 = Comment{
            message = "hello",
            responses = [Comment{message = "goodbye", responses = []}]
          }

    fun expect result (name, decoder, jv) = (
          case (result, decode decoder jv)
           of (Ok x, Ok y) => if (x = y)
                then pr ["# ", name, ": correct\n"]
                else pr ["! ", name, ": wrong result\n"]
            | (Err ex1, Err ex2) => if JU.exnMessage ex1 = JU.exnMessage ex2
                then pr ["# ", name, ": correct error\n"]
                else pr ["! ", name, ": wrong error\n"]
            | (Ok _, Err ex) => pr [
                  "! ", name, ": unexpected error ", JU.exnMessage ex, "\n"
                ]
            | (Err ex, Ok _) => pr [
                  "! ", name, ": expected error ", JU.exnMessage ex, "\n"
                ]
          (* end case *))

    fun expectOk v = expect (Ok v)
    fun expectErr ex = expect (Err ex)

    (* tests *)
    fun run () = (
          (* test basic decoders *)
          expectOk 17 ("t00a", JD.null 17, jv00);
          expectErr (JD.NotNull jv01) ("t00b", JD.null 17, jv01);
          expectOk false ("t01a", JD.bool, jv01);
          expectOk true ("t02a", JD.bool, jv02);
          expectOk 42 ("t03a", JD.int, jv03);
          expectOk 42 ("t03b", JD.intInf, jv03);
          expectOk 42 ("t04a", JD.int, jv04);
          expectOk 42 ("t04b", JD.intInf, jv04);
          expectOk "hello world" ("t06a", JD.string, jv06);
          (* test arrays *)
          expectOk [] ("t07a", JD.array JD.int, jv07);
          expectOk [false] ("t08a", JD.array JD.bool, jv08);
          expectOk [0, 1, 2] ("t09a", JD.array JD.int, jv09);
          expectOk [0, 1, 2] ("t09a", JD.array JD.intInf, jv09);
          (* test object fields *)
          expectOk NONE ("t10a", JD.try (JD.field "fst" JD.int), jv10);
          expectOk 1 ("t11a", JD.field "fst" JD.int, jv11);
          expectOk 2 ("t11b", JD.field "snd" JD.int, jv11);
          expectOk NONE ("t11c", JD.try (JD.field "thd" JD.int), jv11);
          (* array of objects *)
          expectOk "Jane" ("t14a", JD.at [JU.SUB 0, JU.SEL "name"] JD.string, jv14);
          expectOk 42 ("t14b", JD.at [JU.SUB 0, JU.SEL "age"] JD.int, jv14);
          expectOk "Bob" ("t14c", JD.at [JU.SUB 1, JU.SEL "name"] JD.string, jv14);
          expectOk 17 ("t14d", JD.sub 1 (JD.field "age" JD.int), jv14);
          (* required fields *)
          expectOk rv12 ("t15a",
            JD.succeed (fn (n : string) => fn (a : int) => {name=n, age=a})
              |> JD.reqField "name" JD.string
              |> JD.reqField "age" JD.int,
            jv12);
          (* map2 *)
          expectOk rv12 ("t16a",
            JD.map2 (fn (n, a) => {name=n, age=a})
              (JD.field "name" JD.string, JD.field "age" JD.int),
            jv12);
          (* delay *)
          let fun comment () = JD.map2
                (fn (msg, resp) => Comment{message=msg, responses=resp})
                (JD.field "message" JD.string,
                 JD.field "responses" (JD.array (JD.delay comment)))
              in
                expectOk rv15 ("t17", comment (), jv15)
              end)

  end
