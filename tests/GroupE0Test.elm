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
            30000

        old_z80 =
            Z80.constructor

        z80 =
            { old_z80 | pc = addr }

        z80env =
            z80.env

        z80main =
            z80.main

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
            ]
        ]
