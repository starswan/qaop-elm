module CB90Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Address exposing (fromInt, toInt)
import Z80Address exposing (fromInt, toInt)
import Z80Env exposing (mem, setMem)
import Z80Rom


suite : Test
suite =
   let
       addr = 0x5800
       sp = 0xF765
       hl = 0x1234
       old_z80 = Z80.constructor
       old_z80env = old_z80.env
       z80main = old_z80.main
       z80 = { old_z80 | pc = addr  |> fromInt, env = { old_z80env | sp = sp  |> fromInt }, main = { z80main | hl = hl |> fromInt } }

        z80env =
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ test "0xCB 90 RES 2,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x90

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0xFF }
                            }
                in
                Expect.equal ( addr + 2, 0xFB ) ( new_z80.pc|> toInt, new_z80.main.b )
        , test "0xCB 98 RES 3,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x98

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0xFF }
                            }
                in
                Expect.equal ( addr + 2, 0xF7 ) ( new_z80.pc|> toInt, new_z80.main.b )
        , test "0xFD 0xCB nn 0x9E RES 3, (IY + n) -ve" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xFD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0xFE
                            |> setMem (addr + 3) 0x9E
                            |> setMem 0xA07E 0xFF

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0xA080|>fromInt, hl = 0x6545|>fromInt, b = 0xA5 }
                            }

                    mem_value =
                        mem 0xA07E new_z80.env.time z80rom new_z80.env.ram
                in
                Expect.equal ( addr + 4, 0xF7 ) ( new_z80.pc|> toInt, mem_value.value )
        ]
