module Z80Test exposing (..)

import Expect
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
            30000

        clock =
            Z80CoreWithClockTime.constructor

        z80 =
            clock.core

        --z80 =
        --    { old_z80 | pc = addr }
        flags =
            z80.flags

        z80env =
            z80.env

        envwithtime =
            { z80env = z80env, time = clock.clockTime }

        z80main =
            z80.main

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "0xB8 - -xBF CP"
            [ test "0xBC CP H greater" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = envwithtime |> setMemWithTime addr 0xBC |> .z80env
                                    , main = { z80main | hl = 0x0245 }
                                    , flags = { flags | a = 0x06 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, fa = 6, fb = -3, ff = 4, fr = 4 }
                        { pc = new_pc, fa = new_z80.flags.fa, fb = new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            , test "0xBC CP H less" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = envwithtime |> setMemWithTime addr 0xBC |> .z80env
                                    , main = { z80main | hl = 0x0645 }
                                    , flags = { flags | a = 0x02 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, fa = 2, fb = -7, ff = -44, fr = 252 }
                        { pc = new_pc, fa = new_z80.flags.fa, fb = new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            , test "0xBC CP H equal" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = envwithtime |> setMemWithTime addr 0xBC |> .z80env
                                    , main = { z80main | hl = 0x0645 }
                                    , flags = { flags | a = 0x06 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, fa = 6, fb = -7, ff = 0, fr = 0 }
                        { pc = new_pc, fa = new_z80.flags.fa, fb = new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            ]
        ]
