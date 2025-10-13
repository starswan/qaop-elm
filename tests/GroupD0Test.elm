module GroupD0Test exposing (..)

import Expect
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Mem exposing (m1, mem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x8000

        sp =
            0xF765

        clock =
            Z80CoreWithClockTime.constructor

        old_z80 =
            clock.core

        old_z80env =
            old_z80.env

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp } }

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80main =
            z80.main

        flags =
            z80.flags

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "16 bit Pop"
            [ test "POP DE (0xD1)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xD1
                                |> setMemWithTime 0xFF77 0x16
                                |> setMemWithTime 0xFF78 0x56
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal { pc = addr + 1, d = 0x56, e = 0x16, sp = 0xFF79 } { sp = new_z80.env.sp, pc = new_z80.pc, d = new_z80.main.d, e = new_z80.main.e }
            ]
        , describe "0xD2 - JP NC,nn"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xD2
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0100 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal (addr + 3) new_z80.pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xD2
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0200 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal 0x3405 new_z80.pc
            ]
        , test "0xD3 - OUT (N), A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xD3
                            |> setMemWithTime (addr + 1) 0x05
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , flags = { flags | a = 0x39, ff = 0x0200 }
                            }
                            |> Tuple.first
                in
                Expect.equal (addr + 2) new_z80.pc
        , describe "0xD4 CALL NC"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xD4
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0100 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 3, 0xF765 ) ( new_z80.pc, new_z80.env.sp )
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xD4
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0200 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( 0x3405, 0xF763 ) ( new_z80.pc, new_z80.env.sp )
            ]
        , test "0xD7 RST 10" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xD7
                            |> setMemWithTime 0xFF77 0x16
                            |> setMemWithTime 0xFF78 0x56
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }
                            |> Tuple.first
                in
                Expect.equal { pc = 0x10, sp = 0xFF75 } { sp = new_z80.env.sp, pc = new_z80.pc }
        , describe "0xDC CALL C"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDC
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0200 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 3, 0xF765 ) ( new_z80.pc, new_z80.env.sp )
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDC
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0100 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( 0x3405, 0xF763 ) ( new_z80.pc, new_z80.env.sp )
            ]
        , test "0xDB - IN A, (N)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDB
                            |> setMemWithTime (addr + 1) 0x05
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , flags = { flags | a = 0x39, ff = 0x0200 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 2, 0xFF ) ( new_z80.pc, new_z80.flags.a )

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
                            |> setMemWithTime addr 0xDF
                            |> setMemWithTime 0xFF75 0x16
                            |> setMemWithTime 0xFF76 0x56
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }
                            |> Tuple.first

                    lo_value =
                        mem 0xFF75 clock.clockTime z80rom new_z80.env |> .value

                    hi_value =
                        mem 0xFF76 clock.clockTime z80rom new_z80.env |> .value
                in
                Expect.equal { pc = 0x18, sp = 0xFF75, lowmem = 1, highmem = 0x80 } { sp = new_z80.env.sp, pc = new_z80.pc, lowmem = lo_value, highmem = hi_value }
        ]
