module GroupF0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Address exposing (fromInt, toInt)
import Z80Env exposing (m1, setMem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        old_z80 =
            Z80.constructor

        z80 =
            { old_z80 | pc = addr |> fromInt }

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
        [ test "0xF5 PUSH AF" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xF5

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 |> fromInt }
                                , flags = { flags | a = 0x76 }
                                , main = { z80main | hl = 0x5050 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }

                    pushed_low =
                        new_z80.env |> m1 0xFF75 0 z80rom |> .value

                    pushed_high =
                        new_z80.env |> m1 0xFF76 0 z80rom |> .value
                in
                Expect.equal { pc = addr + 1, sp = 0xFF75, push_lo = 0x40, push_hi = 0x76 } { pc = new_z80.pc |> toInt, sp = new_z80.env.sp |> toInt, push_lo = pushed_low, push_hi = pushed_high }
        , describe "0xF9 LD SP,HL"
            [ test "LD SP,HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xF9

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , main = { z80main | hl = 0x5050|> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 1, sp = 0x5050 } { pc = (new_z80.pc |> toInt), sp = new_z80.env.sp |> toInt }
            , test "LD SP,IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xF9

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt}
                                    , main = { z80main | ix = 0x5050|> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, sp = 0x5050 } { pc = new_z80.pc|> toInt, sp = new_z80.env.sp|> toInt }
            , test "LD SP,IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xF9

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77|> fromInt }
                                    , main = { z80main | iy = 0x5050|> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, sp = 0x5050 } { pc = new_z80.pc|> toInt, sp = new_z80.env.sp|> toInt }
            ]
        ]
