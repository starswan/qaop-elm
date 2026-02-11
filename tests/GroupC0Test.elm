module GroupC0Test exposing (..)

import Bitwise exposing (shiftRightBy)
import Expect
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Mem exposing (m1, mem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        sp =
            0xF765

        clock =
            Z80CoreWithClockTime.constructor

        old_z80 =
            clock.core

        old_z80env =
            old_z80.env

        z80 =
            { old_z80 | env = { old_z80env | sp = sp } }

        flags =
            z80.flags

        z80env =
            { z80env = z80.env, time = clock.clockTime }

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
                            |> setMemWithTime addr 0xC1
                            |> setMemWithTime 0xFF77 0x16
                            |> setMemWithTime 0xFF78 0x56
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal { pc = addr + 1, b = 0x56, c = 0x16, sp = 0xFF79 } { sp = new_z80.env.sp, pc = new_pc, b = new_z80.main.b, c = new_z80.main.c }
        , describe "0xC2 - JP NZ,nn"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xC2
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        new_pc =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.third
                    in
                    Expect.equal (addr + 3) new_pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xC2
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal 0x3405 new_pc
            ]
        , describe "0xC4 CALL NZ"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xC4
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0xF765 ) ( new_pc, new_z80.env.sp )
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xC4
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( 0x3405, 0xF763 ) ( new_pc, new_z80.env.sp )
            ]
        , test "0xC6 ADD A,n" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xC6
                            |> setMemWithTime (addr + 1) 0x16
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                , flags = { flags | a = 0x60 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal { pc = addr + 2, a = 0x76 }
                    { pc = new_pc, a = new_z80.flags.a }
        , describe "0xCA - JP Z,nn"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCA
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal (addr + 3) new_pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCA
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 0 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal 0x3405 new_pc
            ]
        , describe "CALL(0xCD) and RET(C9)"
            [ test "Call followed by return" <|
                \_ ->
                    let
                        stackp =
                            0xF000

                        call_site =
                            0x5000

                        new_env =
                            z80env
                                |> setMemWithTime call_site 0xC9
                                |> setMemWithTime (call_site + 1) 0xCD
                                |> setMemWithTime (call_site + 2) (Bitwise.and call_site 0xFF)
                                |> setMemWithTime (call_site + 3) (shiftRightBy 8 call_site)
                                |> .z80env

                        ( z80_1, new_pc ) =
                            { z80
                                | env = { new_env | sp = stackp + 2 }
                                , flags = { flags | a = 0x30 }
                            }
                                |> executeCoreInstruction z80rom (call_site + 1)
                                |> Triple.dropSecond

                        lo_value =
                            z80_1.env |> mem stackp clock.clockTime z80rom.z80rom |> .value

                        high_value =
                            z80_1.env |> mem (stackp + 1) clock.clockTime z80rom.z80rom |> .value

                        ( z80_2, final_pc ) =
                            z80_1 |> executeCoreInstruction z80rom new_pc |> Triple.dropSecond
                    in
                    Expect.equal
                        { addr = call_site, sp1 = stackp, stack_low = 4, stack_high = 0x50, addr4 = call_site + 4, sp2 = stackp + 2 }
                        { addr = new_pc, sp1 = z80_1.env.sp, stack_low = lo_value, stack_high = high_value, addr4 = final_pc, sp2 = z80_2.env.sp }
            ]
        ]
