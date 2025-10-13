module GroupB0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        clock =
            Z80CoreWithClockTime.constructor

        z80 =
            clock.core

        flags =
            z80.flags

        z80env =
            { z80env = z80.env, time = clock.clockTime }

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
                                |> setMemWithTime addr 0xB4
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x03 }
                                    , main = { z80main | hl = 0x5180 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, a = 0x53 } { pc = new_pc, a = new_z80.flags.a }
            , test "OR IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xB4
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x03 }
                                    , main = { z80main | ix = 0x5152, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, a = 0x53 } { pc = new_pc, a = new_z80.flags.a }
            , test "OR IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xB4
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x03 }
                                    , main = { z80main | iy = 0x5152, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, a = 0x53 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0xB5 OR L"
            [ test "OR L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xB5
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | hl = 0x3053 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, a = 0xF3 } { pc = new_pc, a = new_z80.flags.a }
            , test "OR IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xB5
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | ix = 0x5053 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, a = 0xF3 } { pc = new_pc, a = new_z80.flags.a }
            , test "OR IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xB5
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | iy = 0x2053, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, a = 0xF3 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0xB6 OR (HL)"
            [ test "OR (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xB6
                                |> setMemWithTime 0x5050 0x11
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, a = 0x77 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xDD 0xB6 0x01 OR (IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xB6
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x5051 0x11
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | ix = 0x5052, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 3, a = 0x77 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xFD 0xB6 0x01 OR (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xB6
                                |> setMemWithTime (addr + 2) 0x01
                                |> setMemWithTime 0x5051 0x11
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | iy = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 3, a = 0x77 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0xBC CP H"
            [ test "CP H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xBC
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | hl = 0x3053 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, fr = 0x00 } { pc = new_pc, fr = new_z80.flags.fr }
            , test "CP IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xBC
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | ix = 0x3053 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, fr = 0x00 } { pc = new_pc, fr = new_z80.flags.fr }
            , test "CP IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xBC
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | iy = 0x3053 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, fr = 0x00 } { pc = new_pc, fr = new_z80.flags.fr }
            ]
        , describe "0xBD CP L"
            [ test "CP L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xBD
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0xF0 }
                                    , main = { z80main | hl = 0x53F0 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, fr = 0x00 } { pc = new_pc, fr = new_z80.flags.fr }
            , test "CP IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xBD
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | ix = 0x5330 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, fr = 0x00 } { pc = new_pc, fr = new_z80.flags.fr }
            , test "CP IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xBD
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x30 }
                                    , main = { z80main | iy = 0x5330, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, fr = 0x00 } { pc = new_pc, fr = new_z80.flags.fr }
            ]
        , describe "0xBE CP (HL)"
            [ test "CP (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xBE
                                |> setMemWithTime 0x5050 0x11
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x11 }
                                    , main = { z80main | hl = 0x5050 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, fr = 0 } { pc = new_pc, fr = new_z80.flags.fr }
            , test "0xDD 0xBE 0x01 CP (IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xBE
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x5051 0x11
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x11 }
                                    , main = { z80main | ix = 0x5052 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 3, fr = 0x00 } { pc = new_pc, fr = new_z80.flags.fr }
            , test "0xFD 0xBE 0x01 CP (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xBE
                                |> setMemWithTime (addr + 2) 0x01
                                |> setMemWithTime 0x5051 0x11
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x11 }
                                    , main = { z80main | iy = 0x5050 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 3, fr = 0x00 } { pc = new_pc, fr = new_z80.flags.fr }
            ]
        ]
