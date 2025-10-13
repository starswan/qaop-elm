module Group80Test exposing (..)

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
        [ describe "0x84 ADD A, H"
            [ test "ADD A, H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x84
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
                    Expect.equal { pc = addr + 1, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            , test "ADD A,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x84
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
                    Expect.equal { pc = addr + 2, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            , test "ADD A,IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x84
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
                    Expect.equal { pc = addr + 2, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0x85 ADD A, L"
            [ test "ADD A, L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x85
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | hl = 0x3050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            , test "ADD A,IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x85
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
                    Expect.equal { pc = addr + 2, a = 0xC8 } { pc = new_pc, a = new_z80.flags.a }
            , test "ADD A,IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x85
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | iy = 0x2050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0x86 ADD A, (HL)"
            [ test "ADD A, (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x86
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
                    Expect.equal { pc = addr + 1, a = 0x87 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xDD 0x86 0x01 ADD A,(IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x86
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
                    Expect.equal { pc = addr + 3, a = 0x87 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xFD 0x86 0x01 ADD A,(IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x86
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
                    Expect.equal { pc = addr + 3, a = 0x87 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "8 bit loads"
            [ test "0x87 ADD A,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x87
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x02 }
                                    , main = { z80main | hl = 0x6545 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x04 ) ( new_pc, new_z80.flags.a )
            ]
        , describe "0x8C ADC A, H"
            [ test "ADC A, H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x8C
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
                    Expect.equal { pc = addr + 1, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            , test "ADC A,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x8C
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
                    Expect.equal { pc = addr + 2, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            , test "ADC A,IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x8C
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
                    Expect.equal { pc = addr + 2, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0x8D ADC A,L"
            [ test "ADC A,L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x8D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | hl = 0x2050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            , test "ADC A,IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x8D
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
                    Expect.equal { pc = addr + 2, a = 0xC8 } { pc = new_pc, a = new_z80.flags.a }
            , test "ADC A,IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x8D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | iy = 0x2050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, a = 0xC6 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0x8E ADC A, (HL)"
            [ test "ADC A, (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x8E
                                |> setMemWithTime 0x5050 0x11
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76, ff = 0x0100 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, a = 0x88 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xDD 0x8E ADC A,(IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x8E
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x5051 0x11
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76, ff = 0x0100 }
                                    , main = { z80main | ix = 0x5052, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 3, a = 0x88 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xFD 0x8E 0x01 ADC A,(IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x8E
                                |> setMemWithTime (addr + 2) 0x01
                                |> setMemWithTime 0x5051 0x11
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76, ff = 0x0100 }
                                    , main = { z80main | iy = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 3, a = 0x88 } { pc = new_pc, a = new_z80.flags.a }
            ]
        ]
