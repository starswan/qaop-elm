module EDA0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (setMemWithTime)
import Z80Mem exposing (mem)
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

        z80env =
            { z80env = z80.env, time = z80.clockTime }

        z80rom =
            Z80Rom.constructor
    in
    describe "0xED Ax instructions"
        -- Nest as many descriptions as you like.
        [ test "0xEDA2 INI" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xED
                            |> setMemWithTime (addr + 1) 0xA2
                            |> setMemWithTime 0x6545 0x05
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }
                            }

                    mem_value =
                        new_z80.env |> mem 0x6545 new_z80.clockTime z80rom |> .value
                in
                Expect.equal { pc = addr + 2, hl = 0x6546, b = 0xA4, mem = 0xFF } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b, mem = mem_value }
        , test "0xEDA3 OUTI" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xED
                            |> setMemWithTime (addr + 1) 0xA3
                            |> setMemWithTime 0x6545 0x05
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }
                            }
                in
                Expect.equal { pc = addr + 2, hl = 0x6546, b = 0xA4 } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b }
        , test "0xEDAA IND" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xED
                            |> setMemWithTime (addr + 1) 0xAA
                            |> setMemWithTime 0x6545 0x05
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }
                            }

                    mem_value =
                        new_z80.env |> mem 0x6545 new_z80.clockTime z80rom |> .value
                in
                Expect.equal { pc = addr + 2, hl = 0x6544, b = 0xA4, mem = 0xFF } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b, mem = mem_value }
        , test "0xEDAB OUTD" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xED
                            |> setMemWithTime (addr + 1) 0xAB
                            |> setMemWithTime 0x6545 0x05
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }
                            }
                in
                Expect.equal { pc = addr + 2, hl = 0x6544, b = 0xA4 } { pc = new_z80.pc, hl = new_z80.main.hl, b = new_z80.main.b }
        ]
