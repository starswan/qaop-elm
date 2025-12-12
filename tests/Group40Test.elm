module Group40Test exposing (..)

import Dict
import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Rom


suite : Test
suite =
    -- complete 0x40 - 0x4F
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
            Z80Rom.constructor Dict.empty
    in
    describe "Z80.execute_instruction"
        [ test "0x40 LD B,B (NOOP)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x40
                            |> .z80env

                    ( clockTime, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                            }
                            |> Triple.dropFirst
                in
                Expect.equal ( addr + 1, 4 ) ( new_pc, clockTime.cpu_time - clock.clockTime.cpu_time )
        , test "0x41 LD B,C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x41
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, c = 0x76 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_pc, new_z80.main.b )
        , test "0x42 LD B,D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x42
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, d = 0x76 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_pc, new_z80.main.b )
        , test "0x43 LD B,E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x43
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, e = 0x76 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_pc, new_z80.main.b )
        , describe "0x44 LD B,H"
            [ test "LD B,H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x44
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_pc, new_z80.main.b )
            , test "0xDD 0x44 LD B,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x44
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_pc, new_z80.main.b )
            , test "0xFD 0x44 LD B,IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x44
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_pc, new_z80.main.b )
            ]
        , describe "0x45 LD B,L"
            [ test "LD B,L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x45
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x45 ) ( new_pc, new_z80.main.b )
            , test "0xDD 0x45 LD B,IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x45
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_pc, new_z80.main.b )
            , test "0xFD 0x45 LD B,IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x45
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_pc, new_z80.main.b )
            ]
        , describe "0x46 LD B, (HL)"
            [ test "LD B,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x46
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, hl = 0x4546 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x78 ) ( new_pc, new_z80.main.b )
            , test "0xDD46 - LD B,(IX+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x46
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, ix = 0x4547 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( new_pc, new_z80.main.b )
            , test "0xFD46 - LD B,(IY+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x46
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, iy = 0x4547 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( new_pc, new_z80.main.b )
            ]
        , test "0x47 LD B,A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x47
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, e = 0x76 }
                                , flags = { flags | a = 0x38 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x38 ) ( new_pc, new_z80.main.b )
        , test "0x48 LD C,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x48
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0x76 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_pc, new_z80.main.c )
        , test "0x49 LD C,C (NOOP)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x49
                            |> .z80env

                    ( clockTime, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0x76 }
                            }
                            |> Triple.dropFirst
                in
                Expect.equal ( addr + 1, 4 ) ( new_pc, clockTime.cpu_time - clock.clockTime.cpu_time )
        , test "0x4A LD C,D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x4A
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, d = 0x76 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_pc, new_z80.main.c )
        , test "0x4B LD C,E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x4B
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, e = 0x76 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_pc, new_z80.main.c )
        , describe "0x4C"
            [ test "0x4C LD C,H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x4C
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_pc, new_z80.main.c )
            , test "0xDD 0x4C LD C,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x4C
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_pc, new_z80.main.c )
            , test "0xFD 0x4C LD C,IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x4C
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_pc, new_z80.main.c )
            ]
        , describe "0x4D"
            [ test "0x4D LD C,L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x4D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x45 ) ( new_pc, new_z80.main.c )
            , test "0xDD 0x4D LD C,IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x4D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_pc, new_z80.main.c )
            , test "0xFD 0x4D LD C,IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x4D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_pc, new_z80.main.c )
            ]
        , describe "0x4E"
            [ test "0x4E - LD C,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x4E
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, hl = 0x4546 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x78 ) ( new_pc, new_z80.main.c )
            , test "0xDD4E - LD C,(IX+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x4E
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, ix = 0x4547 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( new_pc, new_z80.main.c )
            , test "0xFD4E - LD C,(IY+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x4E
                                |> setMemWithTime (addr + 2) 0x01
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, iy = 0x4545 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( new_pc, new_z80.main.c )
            ]
        , test "0x4F LD C,A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x4F
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, e = 0x76 }
                                , flags = { flags | a = 0x38 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x38 ) ( new_pc, new_z80.main.c )
        ]
