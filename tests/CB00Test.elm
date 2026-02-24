module CB00Test exposing (..)

import Dict
import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMem, setMemWithTime)
import Z80Flags exposing (getFlags)
import Z80Mem exposing (mem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x5800

        addr_plus_1 =
            addr + 1

        sp =
            0xF765

        hl =
            0xE234

        clock =
            Z80CoreWithClockTime.constructor

        old_z80 =
            clock.core

        old_z80env =
            old_z80.env

        z80main =
            old_z80.main

        z80 =
            { old_z80 | env = { old_z80env | sp = sp } |> setMem addr 0xCB clock.clockTime |> Tuple.first, main = { z80main | hl = hl } }

        flags =
            z80.flags

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80rom =
            Z80Rom.constructor Dict.empty
    in
    describe "Bit instructions (CB)"
        [ describe "0xCB 0x00 RLC B"
            [ test "0x50" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_plus_1 0x00
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x50 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0xA0, 0xA4 ) ( new_pc, new_z80.main.b, new_z80.flags |> getFlags )
            , test "0x80" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_plus_1 0x00
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x80 }
                                }
                                |> Triple.dropSecond
                    in
                    -- bit zero and the C flag get the bit that falls off the end
                    Expect.equal ( addr + 2, 0x01, 0x01 ) ( new_pc, new_z80.main.b, new_z80.flags |> getFlags )
            , test "0xFE" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_plus_1 0x00
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0xFE }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0xFD, 0xA9 ) ( new_pc, new_z80.main.b, new_z80.flags |> getFlags )
            , test "0xDD 0xCB d 0x00 RLC (IX + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x00
                                |> setMemWithTime 0x6545 0xFE
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
                    { pc = new_pc, b = new_z80.main.b, mem = mem_value.value, flags = new_z80.flags |> getFlags }
                        |> Expect.equal { pc = addr + 4, b = 0xFD, mem = 0xFD, flags = 0xA9 }
            , test "0xFD 0xCB d 0x00 RLC (IY + d), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x00
                                |> setMemWithTime 0x6545 0xFE
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
                    Expect.equal ( addr + 4, 0xFD, 0xFD ) ( new_pc, new_z80.main.b, mem_value.value )
            ]
        , describe "RLC C"
            [ test "0xCB 0x01 RLC C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_plus_1 0x01
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | c = 0x50 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0xA0 ) ( new_pc, new_z80.main.c )
            , test "0xDD 0xCB d 0x01 RLC (IX + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x01
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
                    Expect.equal ( addr + 4, 0xA0, 0xA0 ) ( new_pc, new_z80.main.c, mem_value.value )
            , test "0xFD 0xCB d 0x01 RLC (IY + d), C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x01
                                |> setMemWithTime 0x6545 0xFE
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
                    { pc = new_pc, c = new_z80.main.c, mem = mem_value.value, flags = new_z80.flags |> getFlags }
                        |> Expect.equal { pc = addr + 4, c = 0xFD, mem = 0xFD, flags = 0xA9 }
            ]
        , test "0xCB 0x02 RLC D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x02
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x50 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_pc, new_z80.main.d )
        , test "0xCB 0x03 RLC E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x03
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | e = 0x50 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_pc, new_z80.main.e )
        , test "0xCB 0x04 RLC H" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x04
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
        , test "0xCB 0x05 RLC L" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x05
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
        , describe "0xCB 0x06 RLC (HL)"
            [ test "0x31" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_plus_1 0x06
                                |> setMemWithTime hl 0x31
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem hl clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 2, 0x62, 0x20 ) ( new_pc, mem_value.value, new_z80.flags |> getFlags )
            , test "0x80" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_plus_1 0x06
                                |> setMemWithTime hl 0x80
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem hl clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 2, 0x01, 0x01 ) ( new_pc, mem_value.value, new_z80.flags |> getFlags )
            , test "0xDD 0xCB 0x06 0x45 RLC (IX + d) 0x31" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime addr_plus_1 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x06
                                |> setMemWithTime 0x6545 0x31
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, b = 0xA5 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x62, 0x20 ) ( new_pc, mem_value.value, new_z80.flags |> getFlags )
            , test "0xDD 0xCB 0x06 0x45 RLC (IX + d) 0x80" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime addr_plus_1 0xCB
                                |> setMemWithTime (addr + 2) 0x45
                                |> setMemWithTime (addr + 3) 0x06
                                |> setMemWithTime 0x6545 0x80
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x01, 0x01 ) ( new_pc, mem_value.value, new_z80.flags |> getFlags )
            , test "0xFD 0xCB 0x06 0x45 RLC (IY - d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime addr_plus_1 0xCB
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime (addr + 3) 0x06
                                |> setMemWithTime 0x6500 0x31
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6501 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6500 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x62, 0x20 ) ( new_pc, mem_value.value, new_z80.flags |> getFlags )
            ]
        , test "0xCB 0x07 RLC A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x07
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
        , test "0xCB 0x08 RRC B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x08
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
        , test "0xCB 0x09 RRC C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x09
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
        , test "0xCB 0x0A RRC D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x0A
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
        , test "0xCB 0x0B RRC E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x0B
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
        , test "0xCB 0x0C RRC H" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x0C
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
                Expect.equal ( addr + 2, 0x2845 ) ( new_pc, new_z80.main.hl )
        , test "0xCB 0x0D RRC L" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x0D
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
                Expect.equal ( addr + 2, 0x5028 ) ( new_pc, new_z80.main.hl )
        , test "0xCB 0x0E RRC (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x0E
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
                Expect.equal ( addr + 2, 0x98 ) ( new_pc, mem_value.value )
        , test "0xCB 0x0F RRC A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr_plus_1 0x0F
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
