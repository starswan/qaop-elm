module CBD0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
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

        clock =
            Z80CoreWithClockTime.constructor

        old_z80 =
            clock.core

        old_z80env =
            old_z80.env

        z80main =
            old_z80.main

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ test "0xCB D0 SET 2,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0xD0
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x00 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 2, 0x04 ) ( new_z80.pc, new_z80.main.b )
        , test "0xDD 0xCB nn 0xD6 SET 2, (IX + n)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x06
                            |> setMemWithTime (addr + 3) 0xD6
                            |> setMemWithTime 0xA086 0x00
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | ix = 0xA080 }
                            }
                            |> Tuple.first

                    mem_value =
                        new_z80.env |> mem 0xA086 clock.clockTime z80rom
                in
                Expect.equal ( addr + 4, 0x04 ) ( new_z80.pc, mem_value.value )
        , test "0xCB D8 SET 3,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0xD8
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x00 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 2, 0x08 ) ( new_z80.pc, new_z80.main.b )
        ]
