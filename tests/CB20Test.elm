module CB20Test exposing (..)

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
            { old_z80 | env = { old_z80env | sp = sp }, main = { z80main | hl = hl, b = 0x35, c = 0xFD, d = 0x6F, e = 0x57 } }

        flags =
            z80.flags

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80rom =
            Z80Rom.constructor Dict.empty
    in
    describe "Bit instructions (CB)"
        [ describe "SLA B"
            [ test "0xCB 0x20 SLA B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x20
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
            , test "0xDD 0xCB d 0x20 SLA (IX + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x20
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
                    Expect.equal ( addr + 4, 0xA0, 0xA0 ) ( new_pc, new_z80.main.b, mem_value.value )
            , test "0xFD 0xCB d 0x20 SLA (IY + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x20
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
                    Expect.equal ( addr + 4, 0xA0, 0xA0 ) ( new_pc, new_z80.main.b, mem_value.value )
            ]
        , describe "SLA C"
            [ test "0xCB 0x21 SLA C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x21
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
            , test "0xDD 0xCB d 0x21 SLA (IX + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x21
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
                    Expect.equal ( addr + 4, 0xA0, 0xA0 ) ( new_pc, new_z80.main.c, mem_value.value )
            , test "0xFD 0xCB d 0x21 SLA (IY + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x21
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, d = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA0, 0xA0 ) ( new_pc, new_z80.main.c, mem_value.value )
            ]
        , describe "SLA D"
            [ test "0xCB 0x22 SLA D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x22
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
            , test "0xDD 0xCB d 0x22 SLA (IX + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x22
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
                    Expect.equal ( addr + 4, 0xA0, 0xA0 ) ( new_pc, new_z80.main.d, mem_value.value )
            , test "0xFD 0xCB d 0x22 SLA (IY + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x22
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, d = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0xA0, 0xA0 ) ( new_pc, new_z80.main.d, mem_value.value )
            ]
        , describe "SLA E"
            [ test "0xCB 0x23 SLA E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x23
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
            , test "0xDD 0xCB d 0x23 SLA (IX + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x23
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
                    Expect.equal ( addr + 4, 0xA0, 0xA0 ) ( new_pc, new_z80.main.e, mem_value.value )
            , test "0xFD 0xCB d 0x23 SLA (IY + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x23
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
                    Expect.equal ( addr + 4, 0xA0, 0xA0 ) ( new_pc, new_z80.main.e, mem_value.value )
            ]
        , describe "SLA H"
            [ test "0xCB 0x24 SLA H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x24
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
                    Expect.equal ( addr + 2, 0xA045 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x24 SLA (IX + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x24
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
                    Expect.equal ( addr + 4, 0xA050, 0xA0 ) ( new_pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x24 SLA (IY + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x24
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
                    Expect.equal ( addr + 4, 0xA050, 0xA0 ) ( new_pc, new_z80.main.hl, mem_value.value )
            ]
        , describe "SLA L"
            [ test "0xCB 0x25 SLA L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x25
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, d = 0x50 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x50A0 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x25 SLA (IX + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x25
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x5000 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x50A0, 0xA0 ) ( new_pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x25 SLA (IY + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x25
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5000 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x50A0, 0xA0 ) ( new_pc, new_z80.main.hl, mem_value.value )
            ]
        , test "0xCB 0x26 SLA (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x26
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
        , test "0xDD 0xCB 0x26 0x45 SLA (IX + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x45
                            |> setMemWithTime (addr + 3) 0x26
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
        , test "0xFD 0xCB 0x26 0x45 SLA (IY + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xFD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x45
                            |> setMemWithTime (addr + 3) 0x26
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
        , test "0xCB 0x27 SLA A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x27
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
        , describe "SRA B"
            [ test "0xCB 0x28 SRA B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x28
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x50 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_pc, new_z80.main.b )
            , test "0xDD 0xCB d 0x28 SRA (IX + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x28
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
            , test "0xFD 0xCB d 0x28 SRA (IY + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x28
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
        , describe "SRA C"
            [ test "0xCB 0x29 SRA C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x29
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, c = 0x50 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_pc, new_z80.main.c )
            , test "0xDD 0xCB d 0x29 SRA (IX + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x29
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, c = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.c, mem_value.value )
            , test "0xFD 0xCB d 0x29 SRA (IY + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x29
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, c = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.c, mem_value.value )
            ]
        , describe "SRA D"
            [ test "0xCB 0x2A SRA D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x2A
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x50 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_pc, new_z80.main.d )
            , test "0xDD 0xCB d 0x2A SRA (IX + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x2A
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
            , test "0xFD 0xCB d 0x2A SRA (IY + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x2A
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, d = 0x50 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_pc, new_z80.main.d, mem_value.value )
            ]
        , describe "SRA E"
            [ test "0xCB 0x2B SRA E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x2B
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x50 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_pc, new_z80.main.e )
            , test "0xDD 0xCB d 0x2B SRA (IX + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x2B
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
            , test "0xFD 0xCB d 0x2B SRA (IY + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x2B
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
        , describe "SRA H"
            [ test "0xCB 0x2C SRA H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x2C
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
            , test "0xDD 0xCB d 0x2C SRA (IX + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x2C
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
            , test "0xFD 0xCB d 0x2C SRA (IY + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x2C
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
        , describe "SRA L"
            [ test "0xCB 0x2D SRA L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime (addr + 1) 0x2D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, d = 0x50 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x5028 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x2D SRA (IX + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x2D
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x5000 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x5028, 0x28 ) ( new_pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x2D SRA (IY + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x2D
                                |> setMemWithTime 0x6545 0x50
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5000 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x5028, 0x28 ) ( new_pc, new_z80.main.hl, mem_value.value )
            ]
        , test "0xCB 0x02E SRA (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x2E
                            |> setMemWithTime 0x6545 0x50
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
                Expect.equal ( addr + 2, 0x28 ) ( new_pc, mem_value.value )
        , test "0xDD 0xCB 0x2E 0x45 SRA (IX + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x45
                            |> setMemWithTime (addr + 3) 0x2E
                            |> setMemWithTime 0x6545 0x50
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
                Expect.equal ( addr + 4, 0x28 ) ( new_pc, mem_value.value )
        , test "0xFD 0xCB 0x2E 0x45 SRA (IY + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xFD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x45
                            |> setMemWithTime (addr + 3) 0x2E
                            |> setMemWithTime 0x6545 0x50
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
                Expect.equal ( addr + 4, 0x28 ) ( new_pc, mem_value.value )
        , test "0xCB 0x2F SRA A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x2F
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
