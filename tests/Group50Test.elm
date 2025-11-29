module Group50Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (setMemWithTime)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        old_z80 =
            Z80.constructor.core

        z80 =
            { old_z80 | pc = addr }

        z80env =
            { z80env = z80.env, time = z80.clockTime }

        z80main =
            z80.main

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "8 bit loads"
            [ test "0x53 LD D,E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x53
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc, new_z80.main.d )
            , test "0x5A LD E,D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x5A
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x34 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x34 ) ( new_z80.pc, new_z80.main.e )
            , test "0x5E LD E, (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x5E
                                |> setMemWithTime 0x6545 0x27
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x34 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x27 ) ( new_z80.pc, new_z80.main.e )
            ]
        , describe "0x54 LD D,H"
            [ test "LD D,H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x54
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_z80.pc, new_z80.main.d )
            , test "0xDD 0x54 LD D,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x54
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_z80.pc, new_z80.main.d )
            , test "0xFD 0x54 LD D,IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x54
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_z80.pc, new_z80.main.d )
            ]
        , describe "0x55 LD D,L"
            [ test "LD D,L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x55
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x45 ) ( new_z80.pc, new_z80.main.d )
            , test "0xDD 0x55 LD D,IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x55
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_z80.pc, new_z80.main.d )
            , test "0xFD 0x55 LD D,IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x55
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_z80.pc, new_z80.main.d )
            ]
        , describe "0x56 LD D, (HL)"
            [ test "LD D,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x56
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, hl = 0x4546 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x78 ) ( z80_after_01.pc, z80_after_01.main.d )
            , test "0xDD56 - LD D,(IX+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x56
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, ix = 0x4547 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc, z80_after_01.main.d )
            , test "0xFD56 - LD D,(IY+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x56
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, iy = 0x4547 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc, z80_after_01.main.d )
            ]
        , describe "0x5C LD E,H"
            [ test "LD E,H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x5C
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_z80.pc, new_z80.main.e )
            , test "0xDD 0x5C LD E,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x5C
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_z80.pc, new_z80.main.e )
            , test "0xFD 0x5C LD E,IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x5C
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_z80.pc, new_z80.main.e )
            ]
        , describe "0x5D LD E,L"
            [ test "LD E,L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x5D
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x45 ) ( new_z80.pc, new_z80.main.e )
            , test "0xDD 0x5D LD E,IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x5D
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_z80.pc, new_z80.main.e )
            , test "0xFD 0x5D LD E,IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x5D
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398, hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_z80.pc, new_z80.main.e )
            ]
        , describe "0x5E LD E,(HL)"
            [ test "LD E,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x5E
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, hl = 0x4546 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x78 ) ( z80_after_01.pc, z80_after_01.main.e )
            , test "0xDD5E - LD E,(IX+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x5E
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, ix = 0x4547 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc, z80_after_01.main.e )
            , test "0xFD5E - LD E,(IY+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x5E
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x4546 0x78
                                |> .z80env

                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, iy = 0x4547 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc, z80_after_01.main.e )
            ]
        ]
