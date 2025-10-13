module CBA8Test exposing (..)

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
        [ test "0xCB A8 RES 5,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0xA8
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0xFF }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 2, 0xDF ) ( new_z80.pc, new_z80.main.b )
        , test "0xFD 0xCB nn 0xAE RES 5, (IY + n) -ve" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xFD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0xFE
                            |> setMemWithTime (addr + 3) 0xAE
                            |> setMemWithTime 0xA07E 0xFF
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0xA080, hl = 0x6545, b = 0xA5 }
                            }
                            |> Tuple.first

                    mem_value =
                        new_z80.env |> mem 0xA07E clock.clockTime z80rom
                in
                Expect.equal ( addr + 4, 0xDF ) ( new_z80.pc, mem_value.value )
        ]
