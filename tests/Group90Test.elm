module Group90Test exposing (..)

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
        [ describe "0x94 SUB H"
            [ test "SUB H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x94
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | hl = 0x5080, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            , test "SUB IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x94
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
                    Expect.equal { pc = addr + 2, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            , test "SUB IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x94
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | iy = 0x5052, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0x95 SUB L"
            [ test "SUB L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x95
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
                    Expect.equal { pc = addr + 1, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            , test "SUB IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x95
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
                    Expect.equal { pc = addr + 2, a = 0x24 } { pc = new_pc, a = new_z80.flags.a }
            , test "SUB IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x95
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
                    Expect.equal { pc = addr + 2, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0x96 SUB (HL)"
            [ test "SUB (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x96
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
                    Expect.equal { pc = addr + 1, a = 0x65 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xDD 0x96 0x01 SUB (IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x96
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
                    Expect.equal { pc = addr + 3, a = 0x65 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xFD 0x96 0x01 SUB (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x96
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
                    Expect.equal { pc = addr + 3, a = 0x65 } { pc = new_pc, a = new_z80.flags.a }
            ]

        --, describe "8 bit loads"
        --    [ test "0x87 ADD A,A" <|
        --        \_ ->
        --            let
        --                new_env =
        --                    z80env
        --                        |> setMem addr 0x87
        --
        --                ( new_z80, new_pc ) =
        --                    execute_instruction z80rom
        --                        { z80
        --                            | env = new_env
        --                            , flags = { flags | a = 0x02 }
        --                            , main = { z80main | hl = 0x6545 }
        --                        }
        --            in
        --            Expect.equal ( addr + 1, 0x04 ) ( new_pc, new_z80.flags.a )
        --    ]
        , describe "0x9C SBC H"
            [ test "SBC H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x9C
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
                    Expect.equal { pc = addr + 1, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            , test "SBC IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x9C
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
                    Expect.equal { pc = addr + 2, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            , test "SBC IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x9C
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , flags = { flags | a = 0x76 }
                                    , main = { z80main | iy = 0x5054, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 2, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0x9D SBC L"
            [ test "SBC L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x9D
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
                    Expect.equal { pc = addr + 1, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            , test "SBC IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x9D
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
                    Expect.equal { pc = addr + 2, a = 0x24 } { pc = new_pc, a = new_z80.flags.a }
            , test "SBC IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x9D
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
                    Expect.equal { pc = addr + 2, a = 0x26 } { pc = new_pc, a = new_z80.flags.a }
            ]
        , describe "0x9E SBC (HL)"
            [ test "SBC (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x9E
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
                    Expect.equal { pc = addr + 1, a = 0x64 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xDD 0x9E SBC (IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x9E
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
                    Expect.equal { pc = addr + 3, a = 0x64 } { pc = new_pc, a = new_z80.flags.a }
            , test "0xFD 0x9E 0x01 SBC (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x9E
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
                    Expect.equal { pc = addr + 3, a = 0x64 } { pc = new_pc, a = new_z80.flags.a }
            ]
        ]
