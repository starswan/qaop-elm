module ED40Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Utils exposing (shiftRightBy8)
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Mem exposing (mem, mem16)
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
            [ test "0xED 0x40 IN B, (C)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x40
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x01, c = 0x02 }
                                    , flags = { flags | a = 0x47 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0xBF ) ( new_pc, new_z80.main.b )
            , test "0xED 0x43 LD (NN), BC" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x43
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x65
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x20, c = 0xF5 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem16 0x6545 z80rom.z80rom clock.clockTime |> .value16
                    in
                    Expect.equal ( addr + 4, 0x20F5 ) ( new_pc, mem_value )
            , test "0xED 0x44 NEG" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x44
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x47 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0xB9 ) ( new_pc, new_z80.flags.a )
            , test "0xED 0x47 LD I,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x47
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x47 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x47 ) ( new_pc, new_z80.interrupts.ir |> shiftRightBy8 )
            , test "0xED 0x48 IN C, (C)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x48
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x01, c = 0x02 }
                                    , flags = { flags | a = 0x47 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0xBF ) ( new_pc, new_z80.main.c )
            , test "0xED 0x4A ADC HL,BC" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x4A
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x01, c = 0x01 }
                                    , flags = { flags | a = 0x47 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6646 ) ( new_pc, new_z80.main.hl )
            , test "0xED 0x4B LD BC,(NN)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x4B
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
                    Expect.equal ( addr + 4, 0xF5, 0x20 ) ( new_pc, new_z80.main.b, new_z80.main.c )
            ]
        ]
