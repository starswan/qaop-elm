module ED50Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Mem exposing (mem16)
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

        flags =
            z80.flags

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80rom =
            Z80Rom.constructor
    in
    describe "0xEn instructions"
        -- Nest as many descriptions as you like.
        [ describe "ED instructions"
            [ test "0xED 0x50 IN D, (C)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x01, c = 0x02 }
                                    , flags = { flags | a = 0x47 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0xBF ) ( new_pc, new_z80.main.d )
            , test "0xED 0x53 LD (NN), DE" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x53
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x65
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x20, e = 0xF5 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem16 0x6545 z80rom.z80rom clock.clockTime |> .value16
                    in
                    Expect.equal ( addr + 4, 0x20F5 ) ( new_pc, mem_value )
            , test "0xED 0x58 IN E, (C)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x58
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x01, c = 0x02 }
                                    , flags = { flags | a = 0x47 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0xBF ) ( new_pc, new_z80.main.e )
            , test "0xED 0x5B LD DE,(NN)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x5B
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x65
                                |> setMemWithTime 0x6545 0x20
                                |> setMemWithTime 0x6546 0xF5
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x01, c = 0x01 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 4, 0xF5, 0x20 ) ( new_pc, new_z80.main.d, new_z80.main.e )
            ]
        ]
