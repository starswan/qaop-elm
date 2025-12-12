module CBA0Test exposing (..)

import Dict
import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
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
            { old_z80 | env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80rom =
            Z80Rom.constructor Dict.empty
    in
    describe "Bit instructions (CB)"
        [ test "0xCB A0 RES 4,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0xA0
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0xFF }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0xEF ) ( new_pc, new_z80.main.b )
        ]
