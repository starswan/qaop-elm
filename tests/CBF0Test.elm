module CBF0Test exposing (..)

import Dict
import Expect exposing (Expectation)
import Test exposing (..)
import Triple
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
            { old_z80 | env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80rom =
            Z80Rom.constructor Dict.empty
    in
    describe "Bit instructions (CB)"
        [ test "0xCB F0 SET 6,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0xF0
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x40 ) ( new_pc, new_z80.main.b )
        , test "0xDD 0xCB nn 0xF6 SET 6, (IX + n)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x06
                            |> setMemWithTime (addr + 3) 0xF6
                            |> setMemWithTime 0xA086 0x00
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | ix = 0xA080 }
                            }
                            |> Triple.dropSecond

                    mem_value =
                        new_z80.env |> mem 0xA086 clock.clockTime z80rom
                in
                Expect.equal ( addr + 4, 0x40 ) ( new_pc, mem_value.value )
        , test "0xCB F8 SET 7,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0xF8
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x80 ) ( new_pc, new_z80.main.b )
        , describe "SET 7 Indirect"
            [ test "0xCB 0xFD SET 7, L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0xFD
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5000 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x5080 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0xFD SET 7, (IX + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0xFD
                                |> setMemWithTime 0x6545 0x00
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x5000 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x5080, 0x80 ) ( new_pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0xFD SET 7, (IY + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0xFD
                                |> setMemWithTime 0x6545 0x00
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5000 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x5080, 0x80 ) ( new_pc, new_z80.main.hl, mem_value.value )
            ]
        ]
