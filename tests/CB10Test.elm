module CB10Test exposing (..)

import Dict
import Expect exposing (Expectation)
import Test exposing (..)
import Triple
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
            { old_z80 | env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        flags =
            z80.flags

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80rom =
            Z80Rom.constructor Dict.empty
    in
    describe "Bit instructions (CB)"
        [ test "0xCB 0x10 RL B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x10
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, b = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_pc, new_z80.main.b )
        , test "0xCB 0x11 RL C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x11
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, c = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_pc, new_z80.main.c )
        , test "0xCB 0x12 RL D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x12
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_pc, new_z80.main.d )
        , test "0xCB 0x13 RL E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x13
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, e = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_pc, new_z80.main.e )
        , test "0xCB 0x14 RL H" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x14
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x5045, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0xA045 ) ( new_pc, new_z80.main.hl )
        , test "0xCB 0x15 RL L" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x15
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x50A0 ) ( new_pc, new_z80.main.hl )
        , test "0xCB 0x16 RL (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x16
                            |> setMemWithTime 0x6545 0x31
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom
                in
                Expect.equal ( addr + 2, 0x62 ) ( new_pc, mem_value.value )
        , test "0xDD 0xCB 0x16 0x45 RL (IX + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x45
                            |> setMemWithTime (addr + 3) 0x16
                            |> setMemWithTime 0x6545 0x31
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | ix = 0x6500, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom
                in
                Expect.equal ( addr + 4, 0x62 ) ( new_pc, mem_value.value )
        , test "0xFD 0xCB 0x16 0x45 RL (IY + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xFD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x45
                            |> setMemWithTime (addr + 3) 0x16
                            |> setMemWithTime 0x6545 0x31
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | iy = 0x6500, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom
                in
                Expect.equal ( addr + 4, 0x62 ) ( new_pc, mem_value.value )
        , test "0xCB 0x17 RL A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x17
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x30 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x60 ) ( new_pc, new_z80.flags.a )
        , describe "RR B"
            [ test "0xCB 0x18 RR B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x18
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, b = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_pc, new_z80.main.b )
            , test "0xDD 0xCB d 0x18 RR (IX + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x18
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, b = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.b, mem_value.value )
            , test "0xFD 0xCB d 0x18 RR (IY + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x18
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, b = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.b, mem_value.value )
            ]
        , describe "RR C"
            [ test "0xCB 0x19 RR C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x19
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, c = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_pc, new_z80.main.c )
            , test "0xDD 0xCB d 0x19 RR (IX + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x19
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, d = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.c, mem_value.value )
            , test "0xFD 0xCB d 0x19 RR (IY + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x19
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, e = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.c, mem_value.value )
            ]
        , describe "RR D"
            [ test "0xCB 0x1A RR D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x1A
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, d = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_pc, new_z80.main.d )
            , test "0xDD 0xCB d 0x1A RR (IX + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x1A
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, d = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.d, mem_value.value )
            , test "0xFD 0xCB d 0x1A RR (IY + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x1A
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, e = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.d, mem_value.value )
            ]
        , describe "RR E"
            [ test "0xCB 0x1B RR E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x1B
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0x50 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_pc, new_z80.main.e )
            , test "0xDD 0xCB d 0x1B RR (IX + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x1B
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, e = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.e, mem_value.value )
            , test "0xFD 0xCB d 0x1B RR (IY + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x1B
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, e = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.e, mem_value.value )
            ]
        , describe "RR H"
            [ test "0xCB 0x1C RR H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x1C
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5045, d = 0x50 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x2845 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x1C RR (IX + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x1C
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x5050 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x2850, 0x28 ) ( new_pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x1C RR (IY + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x1C
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5050 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x2850, 0x28 ) ( new_pc, new_z80.main.hl, mem_value.value )
            ]
        , describe "RR L"
            [ test "0xCB 0x1D RR L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x1D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x5028 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x1D RR (IX + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x1D
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x5050 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x5028, 0x28 ) ( new_pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x1D RR (IY + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x1D
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5050 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x5028, 0x28 ) ( new_pc, new_z80.main.hl, mem_value.value )
            ]
        , test "0xCB 0x1E RR (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x1E
                            |> setMemWithTime 0x6545 0x31
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom
                in
                Expect.equal ( addr + 2, 0x18 ) ( new_pc, mem_value.value )
        , test "0xCB 0x1F RR A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x1F
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x30 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x18 ) ( new_pc, new_z80.flags.a )
        ]
