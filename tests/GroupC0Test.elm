module GroupC0Test exposing (..)

import Bitwise exposing (shiftRightBy)
import Expect
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (m1, setMemIgnoringTime)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        sp =
            0xF765

        old_z80 =
            Z80.constructor.core

        old_z80env =
            old_z80.env

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp } }

        flags =
            z80.flags

        z80env =
            z80.env

        z80main =
            z80.main

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ test "POP BC (0xC1)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xC1
                            |> setMemIgnoringTime 0xFF77 0x16
                            |> setMemIgnoringTime 0xFF78 0x56

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }
                in
                Expect.equal { pc = addr + 1, b = 0x56, c = 0x16, sp = 0xFF79 } { sp = new_z80.env.sp, pc = new_z80.pc, b = new_z80.main.b, c = new_z80.main.c }
        , describe "0xC2 - JP NZ,nn"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemIgnoringTime addr 0xC2
                                |> setMemIgnoringTime (addr + 1) 0x05
                                |> setMemIgnoringTime (addr + 2) 0x34

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal (addr + 3) new_z80.pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemIgnoringTime addr 0xC2
                                |> setMemIgnoringTime (addr + 1) 0x05
                                |> setMemIgnoringTime (addr + 2) 0x34

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                    in
                    Expect.equal 0x3405 new_z80.pc
            ]
        , describe "0xC4 CALL NZ"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemIgnoringTime addr 0xC4
                                |> setMemIgnoringTime (addr + 1) 0x05
                                |> setMemIgnoringTime (addr + 2) 0x34

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0xF765 ) ( new_z80.pc, new_z80.env.sp )
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemIgnoringTime addr 0xC4
                                |> setMemIgnoringTime (addr + 1) 0x05
                                |> setMemIgnoringTime (addr + 2) 0x34

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                    in
                    Expect.equal ( 0x3405, 0xF763 ) ( new_z80.pc, new_z80.env.sp )
            ]
        , test "0xC6 ADD A,n" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xC6
                            |> setMemIgnoringTime (addr + 1) 0x16

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                , flags = { flags | a = 0x60 }
                            }
                in
                Expect.equal { pc = addr + 2, a = 0x76 }
                    { pc = new_z80.pc, a = new_z80.flags.a }
        , describe "0xCA - JP Z,nn"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemIgnoringTime addr 0xCA
                                |> setMemIgnoringTime (addr + 1) 0x05
                                |> setMemIgnoringTime (addr + 2) 0x34

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                    in
                    Expect.equal (addr + 3) new_z80.pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemIgnoringTime addr 0xCA
                                |> setMemIgnoringTime (addr + 1) 0x05
                                |> setMemIgnoringTime (addr + 2) 0x34

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 0 }
                                }
                    in
                    Expect.equal 0x3405 new_z80.pc
            ]
        , describe "CALL(0xCD) and RET(C9)"
            [ test "Call followed by return" <|
                \_ ->
                    let
                        stackp =
                            0xF000

                        start =
                            0x5000

                        new_env =
                            z80env
                                |> setMemIgnoringTime start 0xC9
                                |> setMemIgnoringTime (start + 1) 0xCD
                                |> setMemIgnoringTime (start + 2) (Bitwise.and start 0xFF)
                                |> setMemIgnoringTime (start + 3) (shiftRightBy 8 start)

                        z80_1 =
                            { z80
                                | env = { new_env | sp = stackp + 2 }
                                , pc = start + 1
                                , flags = { flags | a = 0x30 }
                            }
                                |> executeCoreInstruction z80rom

                        lo_value =
                            z80_1.env |> m1 stackp 0 z80rom |> .value

                        high_value =
                            z80_1.env |> m1 (stackp + 1) 0 z80rom |> .value

                        z80_2 =
                            z80_1 |> executeCoreInstruction z80rom
                    in
                    Expect.equal
                        { addr = start, sp1 = stackp, stack_low = 4, stack_high = 0x50, addr4 = start + 4, sp2 = stackp + 2 }
                        { addr = z80_1.pc, sp1 = z80_1.env.sp, stack_low = lo_value, stack_high = high_value, addr4 = z80_2.pc, sp2 = z80_2.env.sp }
            ]
        ]
