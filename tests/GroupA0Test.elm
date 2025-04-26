module GroupA0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
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
        [ describe "0xA4 AND H"
            [ test "AND H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xA4

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x03 }
                                    , main = { z80main | hl = 0x5180 }
                                }
                    in
                    Expect.equal { pc = addr + 1, a = 0x01 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "AND IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xA4

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x03 }
                                    , main = { z80main | ix = 0x5152, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0x01 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "AND IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xA4

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x03 }
                                    , main = { z80main | iy = 0x5152, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0x01 } { pc = new_z80.pc, a = new_z80.flags.a }
            ]
        , describe "0xA5 AND L"
            [ test "AND L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xA5

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | hl = 0x3053 }
                                }
                    in
                    Expect.equal { pc = addr + 1, a = 0x50 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "SUB IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xA5

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | ix = 0x5053 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0x50 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "AND IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xA5

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | iy = 0x2053, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0x50 } { pc = new_z80.pc, a = new_z80.flags.a }
            ]
        , describe "0xA6 AND (HL)"
            [ test "AND (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xA6
                                |> setMem 0x5050 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 1, a = 0x10 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "0xDD 0xA6 0x01 AND (IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xA6
                                |> setMem (addr + 2) 0xFF
                                |> setMem 0x5051 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | ix = 0x5052, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 3, a = 0x10 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "0xFD 0xA6 0x01 AND (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xA6
                                |> setMem (addr + 2) 0x01
                                |> setMem 0x5051 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | iy = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 3, a = 0x10 } { pc = new_z80.pc, a = new_z80.flags.a }
            ]
        , describe "0xAC XOR H"
            [ test "XOR H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xAC

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | hl = 0x3053 }
                                }
                    in
                    Expect.equal { pc = addr + 1, a = 0xC0 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "XOR IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xAC

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | ix = 0x3053 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0xC0 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "XOR IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xAC

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | iy = 0x3053 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0xC0 } { pc = new_z80.pc, a = new_z80.flags.a }
            ]
        , describe "0xAD XOR L"
            [ test "XOR L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xAD

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | hl = 0x5330 }
                                }
                    in
                    Expect.equal { pc = addr + 1, a = 0xC0 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "XOR IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xAD

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | ix = 0x5330 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0xC0 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "XOR IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xAD

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | iy = 0x5330, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 2, a = 0xC0 } { pc = new_z80.pc, a = new_z80.flags.a }
            ]
        , describe "0xAE XOR (HL)"
            [ test "XOR (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xAE
                                |> setMem 0x5050 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | hl = 0x5050 }
                                }
                    in
                    Expect.equal { pc = addr + 1, a = 0x67 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "0xDD 0xAE 0x01 XOR (IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xAE
                                |> setMem (addr + 2) 0xFF
                                |> setMem 0x5051 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | ix = 0x5052 }
                                }
                    in
                    Expect.equal { pc = addr + 3, a = 0x67 } { pc = new_z80.pc, a = new_z80.flags.a }
            , test "0xFD 0xAE 0x01 AND (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xAE
                                |> setMem (addr + 2) 0x01
                                |> setMem 0x5051 0x11

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | iy = 0x5050 }
                                }
                    in
                    Expect.equal { pc = addr + 3, a = 0x67 } { pc = new_z80.pc, a = new_z80.flags.a }
            ]
        ]
