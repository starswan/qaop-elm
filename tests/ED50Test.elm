module ED50Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
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

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x01, c = 0x02 }
                                    , flags = { flags | a = 0x47 }
                                }
                    in
                    Expect.equal ( addr + 2, 0xBF ) ( new_z80.pc, new_z80.main.d )
            , test "0xED 0x58 IN E, (C)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xED
                                |> setMemWithTime (addr + 1) 0x58
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x01, c = 0x02 }
                                    , flags = { flags | a = 0x47 }
                                }
                    in
                    Expect.equal ( addr + 2, 0xBF ) ( new_z80.pc, new_z80.main.e )
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

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x01, c = 0x01 }
                                }
                    in
                    Expect.equal ( addr + 4, 0xF5, 0x20 ) ( new_z80.pc, new_z80.main.d, new_z80.main.e )
            ]
        ]
