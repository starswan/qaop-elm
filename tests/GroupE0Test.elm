module GroupE0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Env exposing (mem16, setMem)
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
            Z80.constructor

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
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "16 bit Pop"
            [ test "POP HL (0xE1)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xE1
                                |> setMem 0xFF77 0x16
                                |> setMem 0xFF78 0x56

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 1, hl = 0x5616, sp = 0xFF79 } { sp = new_z80.env.sp, pc = new_z80.pc, hl = new_z80.main.hl }
            , test "0xFD 0xE1 POP IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xE1
                                |> setMem sp 0x45
                                |> setMem (sp + 1) 0x34

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }
                                }
                    in
                    Expect.equal ( addr + 2, 0x3445, sp + 2 ) ( new_z80.pc, new_z80.main.iy, new_z80.env.sp )
            ]
        , test "0xE3 EX (SP),HL" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xE3
                            |> setMem sp 0x45
                            |> setMem (sp + 1) 0x34

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0xA000 }
                            }
                in
                Expect.equal
                    { pc = new_z80.pc, sp = new_z80.env.sp, hl = new_z80.main.hl, top = (new_z80.env |> mem16 sp z80rom).value }
                    { pc = addr + 1, sp = sp, hl = 0x3445, top = 0xA000 }
        , test "0xDD 0xE3 EX (SP),IX" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0xE3
                            |> setMem sp 0x45
                            |> setMem (sp + 1) 0x34

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | ix = 0xA000 }
                            }
                in
                Expect.equal
                    { pc = new_z80.pc, sp = new_z80.env.sp, ix = new_z80.main.ix, top = (new_z80.env |> mem16 sp z80rom).value }
                    { pc = addr + 2, sp = sp, ix = 0x3445, top = 0xA000 }
        , test "0xE5 PUSH HL" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xE5

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0xA000 }
                            }
                in
                Expect.equal
                    { pc = new_z80.pc, sp = new_z80.env.sp, top = (new_z80.env |> mem16 (sp - 2) z80rom).value }
                    { pc = addr + 1, sp = sp - 2, top = 0xA000 }
        , test "0xFD 0xE5 PUSH IY" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xFD
                            |> setMem (addr + 1) 0xE5

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0xA000 }
                            }
                in
                Expect.equal
                    { pc = new_z80.pc, sp = new_z80.env.sp, top = (new_z80.env |> mem16 (sp - 2) z80rom).value }
                    { pc = addr + 2, sp = sp - 2, top = 0xA000 }
        , test "0xEB (EX DE, HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xEB

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5051, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                , flags = { flags | a = 0x60 }
                            }
                in
                Expect.equal { pc = addr + 1, hl = 0x6000, d = 0x50, e = 0x51 }
                    { pc = new_z80.pc, hl = new_z80.main.hl, d = new_z80.main.d, e = new_z80.main.e }
        ]
