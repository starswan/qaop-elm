module CB80Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (mem, setMemIgnoringTime)
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

        flags =
            old_z80.flags

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        z80env =
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ describe "RES 0,B"
            [ test "0xCB 80 RES 0,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemIgnoringTime addr 0xCB
                                |> setMemIgnoringTime (addr + 1) 0x80

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0xFF }
                                }
                    in
                    Expect.equal ( addr + 2, 0xFE ) ( new_z80.pc, new_z80.main.b )
            , test "0xDD 0xCB d 0x80 RES 0 (IX + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemIgnoringTime addr 0xDD
                                |> setMemIgnoringTime (addr + 1) 0xCB
                                |> setMemIgnoringTime (addr + 2) 0x45
                                |> setMemIgnoringTime (addr + 3) 0x80
                                |> setMemIgnoringTime 0x6545 0xFF

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, b = 0xFF }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0xFE, 0xFE ) ( new_z80.pc, new_z80.main.b, mem_value.value )
            , test "0xFD 0xCB d 0x80 RES 0 (IY + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemIgnoringTime addr 0xFD
                                |> setMemIgnoringTime (addr + 1) 0xCB
                                |> setMemIgnoringTime (addr + 2) 0xFF
                                |> setMemIgnoringTime (addr + 3) 0x80
                                |> setMemIgnoringTime 0x6545 0xFF

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5000 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0xFE, 0xFE ) ( new_z80.pc, new_z80.main.b, mem_value.value )
            ]
        , test "0xCB 81 RES 0,C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xCB
                            |> setMemIgnoringTime (addr + 1) 0x81

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | c = 0xFF }
                            }
                in
                Expect.equal ( addr + 2, 0xFE ) ( new_z80.pc, new_z80.main.c )
        , test "0xCB 82 RES 0,D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xCB
                            |> setMemIgnoringTime (addr + 1) 0x82

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0xFF }
                            }
                in
                Expect.equal ( addr + 2, 0xFE ) ( new_z80.pc, new_z80.main.d )
        , test "0xCB 83 RES 0,E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xCB
                            |> setMemIgnoringTime (addr + 1) 0x83

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | e = 0xFF }
                            }
                in
                Expect.equal ( addr + 2, 0xFE ) ( new_z80.pc, new_z80.main.e )
        , test "0xCB 84 RES 0,H" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xCB
                            |> setMemIgnoringTime (addr + 1) 0x84

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0xFFFF }
                            }
                in
                Expect.equal ( addr + 2, 0xFEFF ) ( new_z80.pc, new_z80.main.hl )
        , test "0xCB 85 RES 0,L" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xCB
                            |> setMemIgnoringTime (addr + 1) 0x85

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0xFFFF }
                            }
                in
                Expect.equal ( addr + 2, 0xFFFE ) ( new_z80.pc, new_z80.main.hl )
        , test "0xCB 0x86 RES 0, (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xCB
                            |> setMemIgnoringTime (addr + 1) 0x86
                            |> setMemIgnoringTime 0xA07E 0xFF

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0xA07E, b = 0xA5 }
                            }

                    mem_value =
                        new_z80.env |> mem 0xA07E new_z80.env.time z80rom
                in
                Expect.equal ( addr + 2, 0xFE ) ( new_z80.pc, mem_value.value )
        , test "0xCB 87 RES 0,A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xCB
                            |> setMemIgnoringTime (addr + 1) 0x87

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , flags = { flags | a = 0xFF }
                                , main = { z80main | hl = 0xFFFF }
                            }
                in
                Expect.equal ( addr + 2, 0xFE ) ( new_z80.pc, new_z80.flags.a )
        ]
