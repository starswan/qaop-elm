module CBB0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Address exposing (fromInt, incrementBy1, incrementBy2, incrementBy3, toInt)
import Z80Env exposing (mem, setMem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x5800
        z80_addr = addr |> fromInt

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
            { old_z80 | pc = addr|>fromInt, env = { old_z80env | sp = sp|>fromInt }, main = { z80main | hl = hl |>fromInt} }

        z80env =
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ test "0xFD 0xCB nn 0xBE RES 7, (IY + n) -ve" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem z80_addr 0xFD
                            |> setMem (z80_addr |> incrementBy1) 0xCB
                            |> setMem (z80_addr |> incrementBy2) 0x06
                            |> setMem ((z80_addr |> incrementBy3)) 0xBE
                            |> setMem (0xA07E |> fromInt) 0xFF

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0xA080|>fromInt, hl = 0x6545|>fromInt, b = 0xA5 }
                            }

                    mem_value =
                        mem (0xA07E |> fromInt) new_z80.env.time z80rom new_z80.env.ram
                in
                Expect.equal ( addr + 4, 0x7F ) ( new_z80.pc|> toInt, mem_value.value )
        ]
