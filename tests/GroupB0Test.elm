module GroupB0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Address exposing (fromInt, toInt)
import Z80Env exposing (setMem)
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
        [ describe "0xB4 OR H"
            [ test "OR H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xB4

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x03 }
                                    , main = { z80main | hl = 0x5180 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 1, a = 0x53 } { pc = new_z80.pc |> toInt, a = new_z80.flags.a }
            , test "OR IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xB4

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , flags = { flags | a = 0x03 }
                                    , main = { z80main | ix = 0x5152 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0x53 } { pc = new_z80.pc |> toInt, a = new_z80.flags.a }
            , test "OR IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xB4

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , flags = { flags | a = 0x03 }
                                    , main = { z80main | iy = 0x5152 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0x53 } { pc = new_z80.pc |> toInt, a = new_z80.flags.a }
            ]
        , describe "0xB5 OR L"
            [ test "OR L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xB5

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | hl = 0x3053 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 1, a = 0xF3 } { pc = new_z80.pc |> toInt, a = new_z80.flags.a }
            , test "OR IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xB5

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | ix = 0x5053 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0xF3 } { pc = new_z80.pc |> toInt, a = new_z80.flags.a }
            , test "OR IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xB5

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | iy = 0x2053 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0xF3 } { pc = new_z80.pc |> toInt, a = new_z80.flags.a }
            ]
        , describe "0xB6 OR (HL)"
            [ test "OR (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xB6
                                |> setMem 0x5050 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | hl = 0x5050 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 1, a = 0x77 } { pc = new_z80.pc |> toInt, a = new_z80.flags.a }
            , test "0xDD 0xB6 0x01 OR (IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xB6
                                |> setMem (addr + 2) 0xFF
                                |> setMem 0x5051 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | ix = 0x5052 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 3, a = 0x77 } { pc = new_z80.pc |> toInt, a = new_z80.flags.a }
            , test "0xFD 0xB6 0x01 OR (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xB6
                                |> setMem (addr + 2) 0x01
                                |> setMem 0x5051 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | iy = 0x5050 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 3, a = 0x77 } { pc = new_z80.pc |> toInt, a = new_z80.flags.a }
            ]
        , describe "0xBC CP H"
            [ test "CP H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xBC

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | hl = 0x3053 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 1, fr = 0x00 } { pc = new_z80.pc |> toInt, fr = new_z80.flags.fr }
            , test "CP IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xBC

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | ix = 0x3053 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 2, fr = 0x00 } { pc = new_z80.pc |> toInt, fr = new_z80.flags.fr }
            , test "CP IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xBC

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | iy = 0x3053 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 2, fr = 0x00 } { pc = new_z80.pc |> toInt, fr = new_z80.flags.fr }
            ]
        , describe "0xBD CP L"
            [ test "CP L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xBD

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | hl = 0x53F0 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 1, fr = 0x00 } { pc = new_z80.pc |> toInt, fr = new_z80.flags.fr }
            , test "CP IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xBD

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | ix = 0x5330 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 2, fr = 0x00 } { pc = new_z80.pc |> toInt, fr = new_z80.flags.fr }
            , test "CP IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xBD

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | iy = 0x5330 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, fr = 0x00 } { pc = new_z80.pc |> toInt, fr = new_z80.flags.fr }
            ]
        , describe "0xBE CP (HL)"
            [ test "CP (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xBE
                                |> setMem 0x5050 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x11 }
                                    , main = { z80main | hl = 0x5050 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 1, fr = 0 } { pc = new_z80.pc |> toInt, fr = new_z80.flags.fr }
            , test "0xDD 0xBE 0x01 CP (IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xBE
                                |> setMem (addr + 2) 0xFF
                                |> setMem 0x5051 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x11 }
                                    , main = { z80main | ix = 0x5052 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 3, fr = 0x00 } { pc = new_z80.pc |> toInt, fr = new_z80.flags.fr }
            , test "0xFD 0xBE 0x01 CP (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xBE
                                |> setMem (addr + 2) 0x01
                                |> setMem 0x5051 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , flags = { flags | a = 0x11 }
                                    , main = { z80main | iy = 0x5050 |> fromInt }
                                }
                    in
                    Expect.equal { pc = addr + 3, fr = 0x00 } { pc = new_z80.pc |> toInt, fr = new_z80.flags.fr }
            ]
        ]
