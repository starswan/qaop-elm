module EDTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (mem, setMem)
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
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "0xEn instructions"
        -- Nest as many descriptions as you like.
        [ describe "ED instructions"
            [ test "0xED 0x6F RLD" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xED
                                |> setMem (addr + 1) 0x6F

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x47 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x40 ) ( new_z80.pc, new_z80.flags.a )
            , test "0xED 0x7B LD SP,(nn)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xED
                                |> setMem (addr + 1) 0x7B
                                |> setMem (addr + 2) 0x98
                                |> setMem (addr + 3) 0xA4
                                |> setMem 0xA498 0x01
                                |> setMem 0xA499 0x02
                                |> setMem 0xA49A 0x03

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x47 }
                                }
                    in
                    Expect.equal ( addr + 4, 0x0201 ) ( new_z80.pc, new_z80.env.sp )
            , test "LDIR ED B0" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xED
                                |> setMem (addr + 1) 0xB0
                                |> setMem 0x5050 0xA0
                                |> setMem 0x5051 0xA5
                                |> setMem 0x5052 0xAA
                                |> setMem 0x5053 0xBA
                                |> setMem 0x5054 0xB5

                        z80_1 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                    , flags = { flags | a = 0x60 }
                                }

                        new_z80 =
                            z80_1 |> executeCoreInstruction z80rom |> executeCoreInstruction z80rom |> executeCoreInstruction z80rom |> executeCoreInstruction z80rom

                        mem_vals =
                            [ (mem 0x6000 new_z80.env.time z80rom new_z80.env).value
                            , (mem 0x6001 new_z80.env.time z80rom new_z80.env).value
                            , (mem 0x6002 new_z80.env.time z80rom new_z80.env).value
                            , (mem 0x6003 new_z80.env.time z80rom new_z80.env).value
                            , (mem 0x6004 new_z80.env.time z80rom new_z80.env).value
                            ]
                    in
                    Expect.equal { pc = addr + 2, b = 0x00, c = 0x00, d = 0x60, e = 0x05, hl = 0x5055, mem = [ 0xA0, 0xA5, 0xAA, 0xBA, 0xB5 ] }
                        { pc = new_z80.pc, b = new_z80.main.b, c = new_z80.main.c, e = new_z80.main.e, d = new_z80.main.d, hl = new_z80.main.hl, mem = mem_vals }
            , test "0xED 78 IN A, (C)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xED
                                |> setMem (addr + 1) 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal { pc = addr + 2, fr = 0xFF, a = 0xFF } { pc = new_z80.pc, fr = new_z80.flags.fr, a = new_z80.flags.a }
            ]
        ]
