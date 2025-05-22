module NonCoreTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (m1, setMem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x8000

        sp =
            0xF765

        old_z80 =
            Z80.constructor.core

        old_z80env =
            old_z80.env

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp } }

        z80env =
            z80.env

        z80main =
            z80.main

        flags =
            z80.flags

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "0xD2 - JP NC,nn"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xD2
                                |> setMem (addr + 1) 0x05
                                |> setMem (addr + 2) 0x34

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0100 }
                                }
                    in
                    Expect.equal (addr + 3) new_z80.pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xD2
                                |> setMem (addr + 1) 0x05
                                |> setMem (addr + 2) 0x34

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0200 }
                                }
                    in
                    Expect.equal 0x3405 new_z80.pc
            ]

        --, test "0xD9 EXX" <|
        --    \_ ->
        --        let
        --            alt =
        --                z80.alt_main
        --
        --            new_env =
        --                z80env
        --                    |> setMem addr 0xD9
        --                    |> setMem (addr + 1) 0x16
        --
        --            new_z80 =
        --                executeSingleInstruction z80rom
        --                    { z80
        --                        | env = { new_env | sp = 0xFF77 }
        --                        , alt_main = { alt | hl = 0x4040, b = 0x67, c = 0x34, d = 0x12, e = 0x81 }
        --                        , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
        --                    }
        --        in
        --        Expect.equal { pc = addr + 1, hl = 0x4040 } { pc = new_z80.pc, hl = new_z80.main.hl }
        , test "0xDF RST 18" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDF
                            |> setMem 0xFF75 0x16
                            |> setMem 0xFF76 0x56

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }

                    lo_value =
                        m1 0xFF75 0 z80rom new_z80.env |> .value

                    hi_value =
                        m1 0xFF76 0 z80rom new_z80.env |> .value
                in
                Expect.equal { pc = 0x18, sp = 0xFF75, lowmem = 1, highmem = 0x80 } { sp = new_z80.env.sp, pc = new_z80.pc, lowmem = lo_value, highmem = hi_value }
        ]
