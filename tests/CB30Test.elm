module CB30Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Mem exposing (mem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x5800

        sp =
            0xF765

        hl =
            0x1234

        clock =
            Z80CoreWithClockTime.constructor

        old_z80 =
            clock.core

        old_z80env =
            old_z80.env

        z80main =
            old_z80.main

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        flags =
            z80.flags

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ describe "SLL B"
            [ test "0xCB 0x30 SLL B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x30
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x50 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0xA1 ) ( new_z80.pc, new_z80.main.b )
            , test "0xDD 0xCB d 0x30 SLL (IX + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x30
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, ix = 0x6500, b = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA1, 0xA1 ) ( new_z80.pc, new_z80.main.b, mem_value.value )
            , test "0xFD 0xCB d 0x30 SLL (IY + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x30
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, iy = 0x6546, b = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA1, 0xA1 ) ( new_z80.pc, new_z80.main.b, mem_value.value )
            ]
        , describe "SLL C"
            [ test "0xCB 0x31 SLL C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x31
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x50 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0xA1 ) ( new_z80.pc, new_z80.main.c )
            , test "0xDD 0xCB d 0x31 SLL (IX + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x31
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, ix = 0x6500, c = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA1, 0xA1 ) ( new_z80.pc, new_z80.main.c, mem_value.value )
            , test "0xFD 0xCB d 0x31 SLL (IY + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x31
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, iy = 0x6546, c = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA1, 0xA1 ) ( new_z80.pc, new_z80.main.c, mem_value.value )
            ]
        , describe "SLL D"
            [ test "0xCB 0x32 SLL D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x32
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, d = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0xA1 ) ( new_z80.pc, new_z80.main.d )
            , test "0xDD 0xCB d 0x32 SLL (IX + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x32
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, ix = 0x6500, d = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA1, 0xA1 ) ( new_z80.pc, new_z80.main.d, mem_value.value )
            , test "0xFD 0xCB d 0x32 SLL (IY + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x32
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, iy = 0x6546, d = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA1, 0xA1 ) ( new_z80.pc, new_z80.main.d, mem_value.value )
            ]
        , describe "SLL E"
            [ test "0xCB 0x33 SLL E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x33
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0xA1 ) ( new_z80.pc, new_z80.main.e )
            , test "0xDD 0xCB d 0x33 SLL (IX + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x33
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, ix = 0x6500, e = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA1, 0xA1 ) ( new_z80.pc, new_z80.main.e, mem_value.value )
            , test "0xFD 0xCB d 0x33 SLL (IY + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x33
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, iy = 0x6546, e = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA1, 0xA1 ) ( new_z80.pc, new_z80.main.e, mem_value.value )
            ]
        , describe "SLL H"
            [ test "0xCB 0x34 SLL H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x34
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5045, d = 0x50 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0xA145 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x34 SLL (IX + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x34
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, ix = 0x6500, b = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA150, 0xA1 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x34 SLL (IY + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x34
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, iy = 0x6546, b = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA150, 0xA1 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            ]
        , describe "SLL L"
            [ test "0xCB 0x35 SLL L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x35
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0x50A1 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x35 SLL (IX + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x35
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, ix = 0x6500, b = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x50A1, 0xA1 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x35 SLL (IY + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x35
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, iy = 0x6546, b = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x50A1, 0xA1 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            ]
        , test "0xCB 0x36 SLL (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x36
                            |> setMemWithTime 0x6545 0x31
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Tuple.first

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom
                in
                Expect.equal ( addr + 2, 0x63 ) ( new_z80.pc, mem_value.value )
        , test "0xDD 0xCB 0x45 0x36 SLL (IX + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x45
                            |> setMemWithTime (addr + 3) 0x36
                            |> setMemWithTime 0x6545 0x31
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | ix = 0x6500, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Tuple.first

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom
                in
                Expect.equal ( addr + 4, 0x63 ) ( new_z80.pc, mem_value.value )
        , test "0xFD 0xCB 0x45 0x36 SLL (IY + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xFD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x45
                            |> setMemWithTime (addr + 3) 0x36
                            |> setMemWithTime 0x6545 0x31
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | iy = 0x6500, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Tuple.first

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom
                in
                Expect.equal ( addr + 4, 0x63 ) ( new_z80.pc, mem_value.value )
        , test "0xCB 0x37 SLL A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x37
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x30 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 2, 0x61 ) ( new_z80.pc, new_z80.flags.a )
        , describe "SRL B"
            [ test "0xCB 0x38 SRL B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x38
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, b = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.b )
            , test "0xDD 0xCB d 0x38 SRL (IX + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x38
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, b = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.b, mem_value.value )
            , test "0xFD 0xCB d 0x38 SRL (IY + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x38
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, b = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.b, mem_value.value )
            ]
        , describe "SRL C"
            [ test "0xCB 0x39 SRL C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x39
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, c = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.c )
            , test "0xDD 0xCB d 0x39 SRL (IX + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x39
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, c = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.c, mem_value.value )
            , test "0xFD 0xCB d 0x39 SRL (IY + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x39
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, c = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.c, mem_value.value )
            ]
        , describe "SRL D"
            [ test "0xCB 0x3A SRL D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x3A
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, d = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.d )
            , test "0xDD 0xCB d 0x3A SRL (IX + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x3A
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, d = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.d, mem_value.value )
            , test "0xFD 0xCB d 0x3A SRL (IY + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x3A
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, d = 0x50 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.d, mem_value.value )
            ]
        , describe "SRL E"
            [ test "0xCB 0x3B SRL E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x3B
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.e )
            ]
        , test "0xDD 0xCB d 0x3B SRL (IX + d), E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x45
                            |> setMemWithTime (addr + 3) 0x3B
                            |> setMemWithTime 0x6545 0x50
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | ix = 0x6500, e = 0x50 }
                            }
                            |> Tuple.first

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom
                in
                Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.e, mem_value.value )
        , test "0xFD 0xCB d 0x3B SRL (IY + d), E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xFD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0xFF
                            |> setMemWithTime (addr + 3) 0x3B
                            |> setMemWithTime 0x6545 0x50
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0x6546, e = 0x50 }
                            }
                            |> Tuple.first

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom
                in
                Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.e, mem_value.value )
        , describe "SRL H"
            [ test "0xCB 0x3C SRL H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x3C
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x5045, d = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0x2845 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x3C SRL (IX + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x3C
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x5050 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x2850, 0x28 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x3C SRL (IY + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x3C
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5050 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x2850, 0x28 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            ]
        , describe "SRL L"
            [ test "0xCB 0x3D SRL L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x3D
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x5050, d = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0x5028 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x3D SRL (IX + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x3D
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x5000 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x5028, 0x28 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x3D SRL (IY + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x3D
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5000 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x5028, 0x28 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            ]
        , describe "SRL (HL)"
            [ test "0xCB 0x03E SRL (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x3E
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0xCB 0x3E 0x45 SRL (IX + d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x3E
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6500, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28 ) ( new_z80.pc, mem_value.value )
            , test "0xFD 0xCB 0x3E 0x45 SRL (IY + d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x3E
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | iy = 0x6500, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Tuple.first

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28 ) ( new_z80.pc, mem_value.value )
            ]
        , test "0xCB 0x3F SRL A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x3F
                            |> .z80env

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x30 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 2, 0x18 ) ( new_z80.pc, new_z80.flags.a )
        ]
