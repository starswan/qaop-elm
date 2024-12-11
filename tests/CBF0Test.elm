module CBF0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Address exposing (fromInt, toInt)
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
            Z80.constructor

        old_z80env =
            old_z80.env

        z80main =
            old_z80.main

        z80 =
            { old_z80 | pc = addr |> fromInt, env = { old_z80env | sp = sp |> fromInt }, main = { z80main | hl = hl |> fromInt } }

        z80env =
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ test "0xDD 0xCB nn 0xF6 SET 6, (IX + n)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0x06
                            |> setMem (addr + 3) 0xF6
                            |> setMem 0xA086 0x00

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | ix = 0xA080 |> fromInt }
                            }

                    mem_value =
                        mem 0xA086 new_z80.env.time z80rom new_z80.env.ram
                in
                Expect.equal ( addr + 4, 0x40 ) ( new_z80.pc |> toInt, mem_value.value )
        ]
