module Group60Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Env exposing (mem, setMem)
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
        [ describe "0x60"
            [ test "0x60 LD H,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x60

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x7645 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x60 LD IXH,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x60

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x60 LD IYH,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x60

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, hl = 0x6545, b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645, 0x6545 ) ( new_z80.pc, new_z80.main.iy, new_z80.main.hl )
            ]
        , describe "0x61"
            [ test "0x61 LD H,C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x61

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x7645 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x61 LD IXH,C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x61

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x61 LD IYH,C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x61

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x62"
            [ test "0x62 LD H,D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x62

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x7645 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x62 LD IXH,D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x62

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, d = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x62 LD IYH,D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x62

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, d = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x63"
            [ test "0x63 LD H,E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x63

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x7645 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x63 LD IXH,E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x63

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, e = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x63 LD IYH,E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x63

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, e = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x65"
            [ test "0x65 LD H,L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x65

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x4545 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x65 LD IXH,IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x65

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, hl = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x4545 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x65 LD IYH,IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x65

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, hl = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x4545 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x66"
            [ test "0x66 LD H,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x66
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x7845 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x66 LD H,(IX+m)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x66
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6543, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x7845, 0x6543 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x66 LD H,(IY+m)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x66
                                |> setMem (addr + 2) 0xFE
                                |> setMem 0x6541 0x78

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | iy = 0x6543, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x7845 ) ( new_z80.pc, new_z80.main.hl )
            ]
        , describe "0x67"
            [ test "0x67 LD H,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x67

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x3945 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x67 LD IXH,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x67

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, c = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x3945 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x67 LD IYH,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x67

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, c = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x3945 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x68"
            [ test "0x68 LD L,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x68

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x6576 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x68 LD IXL,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x68

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, b = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6576 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x68 LD IYL,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x68

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, b = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6576 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x69"
            [ test "0x69 LD L,C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x69

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x6576 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x69 LD IXL,C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x69

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, c = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6576 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x69 LD IYL,C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x69

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, c = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6576 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x6A"
            [ test "0x6A LD L,D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x6A

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x6576 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x6A LD IXL,D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x6A

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, d = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6576 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x6A LD IYL,D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x6A

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, d = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6576 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x6B"
            [ test "0x6A LD L,E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x6B

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x6576 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x6B LD IXL,E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x6B

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, e = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6576 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x6B LD IYL,E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x6B

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, e = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6576 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x6C"
            [ test "0x6C LD L,H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x6C

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x6565 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x6C LD IXL,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x6C

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, e = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6565 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x6C LD IYL,IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x6C

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, e = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6565 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "0x6E"
            [ test "0x6E LD L,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x6E
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x6578 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x6E LD L,(IX+m)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x6E
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6543, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x6578 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xFD 0x6E LD L,(IY+m)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x6E
                                |> setMem (addr + 2) 0xFE
                                |> setMem 0x6541 0x78

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | iy = 0x6543, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x6578 ) ( new_z80.pc, new_z80.main.hl )
            ]
        , describe "0x6F"
            [ test "0x6F LD L,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x6F

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x6F }
                                    , main = { z80main | hl = 0x6545 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x656F ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x6F LD IXL,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x6F

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, e = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6539 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x6F LD IYL,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x6F

                        new_z80 =
                            executeSingleInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, e = 0x76 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6539 ) ( new_z80.pc, new_z80.main.iy )
            ]
        ]
