module GroupF0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem16, setMem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        old_z80 =
            Z80.constructor

        z80 =
            { old_z80 | pc = addr }

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
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , flags = { flags | a = 0x76 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }

                    pushed =
                        new_z80.env |> mem16 0xFF75 z80rom
                in
                Expect.equal { pc = addr + 1, sp = 0xFF75, push = 0x7640 } { pc = new_z80.pc, sp = new_z80.env.sp, push = pushed.value }
        , describe "0xF9 LD SP,HL"
            [
            test "LD SP,HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xF9

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 1, sp = 0x5050 } { pc = new_z80.pc, sp = new_z80.env.sp }
            ,test "LD SP,IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr  + 1 ) 0xF9

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | ix = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, sp = 0x5050 } { pc = new_z80.pc, sp = new_z80.env.sp }
            ,test "LD SP,IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr  + 1 ) 0xF9

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | iy = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, sp = 0x5050 } { pc = new_z80.pc, sp = new_z80.env.sp }
            ]
        ]
