module Group70Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (mem, setMem)
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
        [ describe "0x70"
            [ test "0x70 LD (HL),B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x70

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 1, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0x70 LD (IX+m), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x70
                                |> setMem (addr + 2) 0xFE
                                |> setMem 0x6541 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x2545, ix = 0x6543, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6541 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xFD 0x70 LD (IY+m), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x70
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x2545, iy = 0x6543, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            ]
        , describe "0x71"
            [ test "0x71 LD (HL),C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x71

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, c = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 1, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0x71 LD (IX+m), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x71
                                |> setMem (addr + 2) 0xFE
                                |> setMem 0x6541 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6543, c = 0xA5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6541 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xFD 0x71 LD (IY+m), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x71
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6543, c = 0xA5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            ]
        , describe "0x72"
            [ test "0x72 LD (HL),D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x72

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, d = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 1, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0x72 LD (IX+m), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x72
                                |> setMem (addr + 2) 0xFE
                                |> setMem 0x6541 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6543, d = 0xA5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6541 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xFD 0x72 LD (IY+m), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x72
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6543, d = 0xA5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            ]
        , describe "0x73"
            [ test "0x73 LD (HL),E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x73

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 1, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0x73 LD (IX+m), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x73
                                |> setMem (addr + 2) 0xFE
                                |> setMem 0x6541 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6543, e = 0xA5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6541 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xFD 0x73 LD (IY+m), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x73
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6543, e = 0xA5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            ]
        , describe "0x74"
            [ test "0x74 LD (HL),H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x74

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0x74 LD (IX+m), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x74
                                |> setMem (addr + 2) 0xFE
                                |> setMem 0x6541 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6543, hl = 0xA5F5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6541 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xFD 0x74 LD (IY+m), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x74
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6543, hl = 0xA5F5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            ]
        , describe "0x75"
            [ test "0x75 LD (HL),L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x75

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 1, 0x45 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0x75 LD (IX+m), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x75
                                |> setMem (addr + 2) 0xFE
                                |> setMem 0x6541 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6543, hl = 0xA5F5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6541 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xF5 ) ( new_z80.pc, mem_value.value )
            , test "0xFD 0x75 LD (IY+m), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x75
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6543, hl = 0xA5F5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 3, 0xF5 ) ( new_z80.pc, mem_value.value )
            ]
        , describe "0x78"
            [ test "0x78 LD A,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x78

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc, new_z80.flags.a )
            ]
        , describe "0x7C"
            [ test "0x7C LD A,H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x7C

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_z80.pc, new_z80.flags.a )
            , test "0xDD 0x7C LD A, IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x7C

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6543, hl = 0xA5F5 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x65 ) ( new_z80.pc, new_z80.flags.a )
            , test "0xFD 0x7C LD A, IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x7C

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6543, hl = 0xA5F5 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x65 ) ( new_z80.pc, new_z80.flags.a )
            ]
        , describe "0x7D"
            [ test "0x7D LD A,L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x7D

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x45 ) ( new_z80.pc, new_z80.flags.a )
            , test "0xDD 0x7D LD A, IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x7D

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6543, hl = 0xA5F5 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x43 ) ( new_z80.pc, new_z80.flags.a )
            , test "0xFD 0x7D LD A, IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x7D

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6543, hl = 0xA5F5 }
                                }

                        --mem_value =
                        --   new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 2, 0x43 ) ( new_z80.pc, new_z80.flags.a )
            ]
        , describe "0x7E LD A, (HL)"
            [ test "LD A,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x7E
                                |> setMem 0x4546 0x78

                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, hl = 0x4546 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x78 ) ( z80_after_01.pc, z80_after_01.flags.a )
            , test "0xDD7E - LD A,(IX+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x7E
                                |> setMem (addr + 2) 0xFF
                                |> setMem 0x4546 0x78

                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, ix = 0x4547 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc, z80_after_01.flags.a )
            , test "0xFD7E - LD A,(IY+d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x7E
                                |> setMem (addr + 2) 0xFF
                                |> setMem 0x4546 0x78

                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46, iy = 0x4547 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x78 ) ( z80_after_01.pc, z80_after_01.flags.a )
            ]
        ]
