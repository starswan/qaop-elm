module Group40Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Address exposing (fromInt, toInt)
import Z80Address exposing (fromInt, toInt)
import Z80Env exposing (setMem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        z80_addr =
            addr |> fromInt

        old_z80 =
            Z80.constructor

        z80 =
            { old_z80 | pc = z80_addr }

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
        -- 0x40 is a NO-OP
        [ test "0x41 LD B,C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem z80_addr 0x41

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, c = 0x76 }
                            }
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.main.b )
        , test "0x42 LD B,D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x42

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, d = 0x76 }
                            }
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.main.b )
        , test "0x43 LD B,E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x43

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, e = 0x76 }
                            }
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.main.b )
        , describe "0x44 LD B,H"
            [ test "LD B,H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0x44

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_z80.pc |> toInt, new_z80.main.b )
            , test "0xDD 0x44 LD B,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xDD
                                |> setMem (addr + 1 |> fromInt) 0x44

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398 |> fromInt, hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_z80.pc |> toInt, new_z80.main.b )
            , test "0xFD 0x44 LD B,IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xFD
                                |> setMem (addr + 1 |> fromInt) 0x44

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398 |> fromInt, hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_z80.pc |> toInt, new_z80.main.b )
            ]
        , describe "0x45 LD B,L"
            [ test "LD B,L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0x45

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x45 ) ( new_z80.pc |> toInt, new_z80.main.b )
            , test "0xDD 0x45 LD B,IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xDD
                                |> setMem (addr + 1 |> fromInt) 0x45

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398 |> fromInt, hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_z80.pc |> toInt, new_z80.main.b )
            , test "0xFD 0x45 LD B,IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xFD
                                |> setMem (addr + 1 |> fromInt) 0x45

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398 |> fromInt, hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_z80.pc |> toInt, new_z80.main.b )
            ]
        , describe "0x46 LD B, (HL)"
            [ test "LD B,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0x46
                                |> setMem (0x4546 |> fromInt) 0x78

                        z80_after_01 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, hl = 0x4546 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 1, 0x78 ) ( z80_after_01.pc |> toInt, z80_after_01.main.b )
            , test "0xDD46 - LD B,(IX+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xDD
                                |> setMem (addr + 1 |> fromInt) 0x46
                                |> setMem (addr + 2 |> fromInt) 0xFF
                                |> setMem (0x4546 |> fromInt) 0x78

                        z80_after_01 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, ix = 0x4547 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc |> toInt, z80_after_01.main.b )
            , test "0xFD46 - LD B,(IY+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xFD
                                |> setMem (addr + 1 |> fromInt) 0x46
                                |> setMem (addr + 2 |> fromInt) 0xFF
                                |> setMem (0x4546 |> fromInt) 0x78

                        z80_after_01 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, iy = 0x4547 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc |> toInt, z80_after_01.main.b )
            ]
        , test "0x47 LD B,A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x47

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, e = 0x76 }
                                , flags = { flags | a = 0x38 }
                            }
                in
                Expect.equal ( addr + 1, 0x38 ) ( new_z80.pc |> toInt, new_z80.main.b )
        , test "0x48 LD C,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem z80_addr 0x48

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, b = 0x76 }
                            }
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.main.c )
        , test "0x4A LD C,D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x4A

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, d = 0x76 }
                            }
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.main.c )
        , test "0x4B LD C,E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x4B

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, e = 0x76 }
                            }
                in
                Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.main.c )
        , describe "0x4C"
            [ test "0x4C LD C,H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0x4C

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_z80.pc |> toInt, new_z80.main.c )
            , test "0xDD 0x4C LD C,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xDD
                                |> setMem (addr + 1 |> fromInt) 0x4C

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398 |> fromInt, hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_z80.pc |> toInt, new_z80.main.c )
            , test "0xFD 0x4C LD C,IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xFD
                                |> setMem (addr + 1 |> fromInt) 0x4C

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398 |> fromInt, hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_z80.pc |> toInt, new_z80.main.c )
            ]
        , describe "0x4D"
            [ test "0x4D LD C,L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0x4D

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x45 ) ( new_z80.pc |> toInt, new_z80.main.c )
            , test "0xDD 0x4D LD C,IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xDD
                                |> setMem (addr + 1 |> fromInt) 0x4D

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398 |> fromInt, hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_z80.pc |> toInt, new_z80.main.c )
            , test "0xFD 0x4D LD C,IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xFD
                                |> setMem (addr + 1 |> fromInt) 0x4D

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x2398 |> fromInt, hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x98 ) ( new_z80.pc |> toInt, new_z80.main.c )
            ]
        , describe "0x4E"
            [ test "0x4E - LD C,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0x4E
                                |> setMem (0x4546 |> fromInt) 0x78

                        z80_after_01 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, hl = 0x4546 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 1, 0x78 ) ( z80_after_01.pc |> toInt, z80_after_01.main.c )
            , test "0xDD4E - LD C,(IX+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xDD
                                |> setMem (addr + 1 |> fromInt) 0x4E
                                |> setMem (addr + 2 |> fromInt) 0xFF
                                |> setMem (0x4546 |> fromInt) 0x78

                        z80_after_01 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, ix = 0x4547 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc |> toInt, z80_after_01.main.c )
            , test "0xFD4E - LD C,(IY+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem z80_addr 0xFD
                                |> setMem (addr + 1 |> fromInt) 0x4E
                                |> setMem (addr + 2 |> fromInt) 0x01
                                |> setMem (0x4546 |> fromInt) 0x78

                        z80_after_01 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, iy = 0x4545 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc |> toInt, z80_after_01.main.c )
            ]
        , test "0x4F LD C,A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x4F

                    new_z80 =
                        executeSingleInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, e = 0x76 }
                                , flags = { flags | a = 0x38 }
                            }
                in
                Expect.equal ( addr + 1, 0x38 ) ( new_z80.pc |> toInt, new_z80.main.c )
        ]
