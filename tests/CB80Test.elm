module CB80Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Address exposing (fromInt, incrementBy1, incrementBy2, incrementBy3, toInt)
import Z80Env exposing (mem, setMem)
import Z80Rom


suite : Test
suite =
    let
        addr_int = 0x5800

        addr =
            addr_int |> fromInt

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
            { old_z80 | pc = addr, env = { old_z80env | sp = sp|>fromInt }, main = { z80main | hl = hl|>fromInt } }

        z80env =
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ test "0xCB 80 RES 0,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr |> incrementBy1) 0x80

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0xFF }
                            }
                in
                Expect.equal ( addr_int + 2, 0xFE ) ( new_z80.pc|> toInt, new_z80.main.b )
        , test "0xCB 81 RES 0,C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr |> incrementBy1) 0x81

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | c = 0xFF }
                            }
                in
                Expect.equal ( addr_int + 2, 0xFE ) ( new_z80.pc|> toInt, new_z80.main.c )
        , test "0xCB 88 RES 1,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr |> incrementBy1) 0x88

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0xFF }
                            }
                in
                Expect.equal ( addr_int + 2, 0xFD ) ( new_z80.pc|> toInt, new_z80.main.b )
        , test "0xFD 0xCB nn 0x8E RES 1, (IY + n) -ve" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xFD
                            |> setMem (addr |> incrementBy1) 0xCB
                            |> setMem (addr |> incrementBy2) 0xFE
                            |> setMem (addr |> incrementBy3) 0x8E
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
                Expect.equal ( addr_int + 4, 0xFD ) ( new_z80.pc|> toInt, mem_value.value )
        ]
