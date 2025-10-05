module EDB0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (mem, setMemWithTime)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x5800

        sp =
            0xF765

        hl =
            0x1234

        old_z80 =
            Z80.constructor.core

        old_z80env =
            old_z80.env

        z80main =
            old_z80.main

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        flags =
            z80.flags

        z80env =
            { z80env = z80.env, time = z80.clockTime }

        z80rom =
            Z80Rom.constructor
    in
    describe "ED instructions"
        [ test "LDIR ED B0" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xED
                            |> setMemWithTime (addr + 1) 0xB0
                            |> setMemWithTime 0x5050 0xA0
                            |> setMemWithTime 0x5051 0xA5
                            |> setMemWithTime 0x5052 0xAA
                            |> setMemWithTime 0x5053 0xBA
                            |> setMemWithTime 0x5054 0xB5
                            |> .z80env

                    z80_1 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }

                    new_z80 =
                        z80_1 |> executeCoreInstruction z80rom |> executeCoreInstruction z80rom |> executeCoreInstruction z80rom |> executeCoreInstruction z80rom

                    mem_vals =
                        [ (mem 0x6000 new_z80.clockTime z80rom new_z80.env).value
                        , (mem 0x6001 new_z80.clockTime z80rom new_z80.env).value
                        , (mem 0x6002 new_z80.clockTime z80rom new_z80.env).value
                        , (mem 0x6003 new_z80.clockTime z80rom new_z80.env).value
                        , (mem 0x6004 new_z80.clockTime z80rom new_z80.env).value
                        ]
                in
                Expect.equal { pc = addr + 2, b = 0x00, c = 0x00, d = 0x60, e = 0x05, hl = 0x5055, mem = [ 0xA0, 0xA5, 0xAA, 0xBA, 0xB5 ] }
                    { pc = new_z80.pc, b = new_z80.main.b, c = new_z80.main.c, e = new_z80.main.e, d = new_z80.main.d, hl = new_z80.main.hl, mem = mem_vals }
        , describe "0xEDB2 INIR"
            [ test "Not looping" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0xB2
                                |> setMemWithTime 0x6545 0x05
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x01, c = 0x5F }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.clockTime z80rom |> .value
                    in
                    Expect.equal { pc = addr + 2, hl = 0x6546, b = 0x00, mem = 0xFF } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b, mem = mem_value }
            , test "Looping" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0xB2
                                |> setMemWithTime 0x6545 0x05
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x02, c = 0x5F }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.clockTime z80rom |> .value
                    in
                    Expect.equal { pc = addr, hl = 0x6546, b = 0x01, mem = 0xFF } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b, mem = mem_value }
            ]
        , describe "0xEDB3 OTIR"
            [ test "Not looping" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0xB3
                                |> setMemWithTime 0x6545 0x05
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x01, c = 0x5F }
                                }
                    in
                    Expect.equal { pc = addr + 2, hl = 0x6546, b = 0x00 } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b }
            , test "Looping" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0xB2
                                |> setMemWithTime 0x6545 0x05
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x02, c = 0x5F }
                                }
                    in
                    Expect.equal { pc = addr, hl = 0x6546, b = 0x01 } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b }
            ]
        , describe "0xEDBA INDR"
            [ test "Not looping" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0xBA
                                |> setMemWithTime 0x6545 0x05
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x01, c = 0x5F }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.clockTime z80rom |> .value
                    in
                    Expect.equal { pc = addr + 2, hl = 0x6544, b = 0x00, mem = 0xFF } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b, mem = mem_value }
            , test "Looping" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0xBA
                                |> setMemWithTime 0x6545 0x05
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x02, c = 0x5F }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.clockTime z80rom |> .value
                    in
                    Expect.equal { pc = addr, hl = 0x6544, b = 0x01, mem = 0xFF } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b, mem = mem_value }
            ]
        , describe "0xEDBB OTDR"
            [ test "Not looping" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0xBB
                                |> setMemWithTime 0x6545 0x05
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x01, c = 0x5F }
                                }
                    in
                    Expect.equal { pc = addr + 2, hl = 0x6544, b = 0x00 } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b }
            , test "Looping" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0xBB
                                |> setMemWithTime 0x6545 0x05
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x02, c = 0x5F }
                                }
                    in
                    Expect.equal { pc = addr, hl = 0x6544, b = 0x01 } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b }
            ]
        ]
