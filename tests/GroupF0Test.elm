module GroupF0Test exposing (..)

import Expect
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Mem exposing (mem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        clock =
            Z80CoreWithClockTime.constructor

        old_z80 =
            clock.core

        z80 =
            { old_z80 | pc = addr }

        flags =
            z80.flags

        z80env =
            z80.env

        envwithtime =
            { z80env = z80env, time = clock.clockTime }

        z80main =
            z80.main

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ test "0xF5 PUSH AF" <|
            \_ ->
                let
                    new_env =
                        envwithtime
                            |> setMemWithTime addr 0xF5
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , flags = { flags | a = 0x76 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }
                            |> Tuple.first

                    pushed_low =
                        new_z80.env |> mem 0xFF75 clock.clockTime z80rom |> .value

                    pushed_high =
                        new_z80.env |> mem 0xFF76 clock.clockTime z80rom |> .value
                in
                Expect.equal { pc = addr + 1, sp = 0xFF75, push_lo = 0x40, push_hi = 0x76 } { pc = new_z80.pc, sp = new_z80.env.sp, push_lo = pushed_low, push_hi = pushed_high }
        , describe "0xF9 LD SP,HL"
            [ test "LD SP,HL" <|
                \_ ->
                    let
                        new_env =
                            envwithtime
                                |> setMemWithTime addr 0xF9
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal { pc = addr + 1, sp = 0x5050 } { pc = new_z80.pc, sp = new_z80.env.sp }
            , test "LD SP,IX" <|
                \_ ->
                    let
                        new_env =
                            envwithtime
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xF9
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | ix = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal { pc = addr + 2, sp = 0x5050 } { pc = new_z80.pc, sp = new_z80.env.sp }
            , test "LD SP,IY" <|
                \_ ->
                    let
                        new_env =
                            envwithtime
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xF9
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | iy = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal { pc = addr + 2, sp = 0x5050 } { pc = new_z80.pc, sp = new_z80.env.sp }
            ]
        ]
