module CB58Test exposing (..)

import Dict
import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMem, setMemWithTime)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x5800

        addr_1 =
            addr + 1

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
            { z80env = z80.env |> setMem addr 0xCB clock.clockTime |> Tuple.first, time = clock.clockTime }

        z80rom =
            Z80Rom.constructor Dict.empty
    in
    describe "Bit instructions (CB)"
        [ describe "0xCB 0x58 BIT 3,B"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_1 0x58
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, b = 0x00 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xCB
                                |> setMemWithTime addr_1 0x58
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, b = 0x08 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x59 BIT 3,C"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_1 0x59
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, c = 0x00 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, False ) ( new_pc, new_z80.flags.fr /= 0 )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_1 0x59
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, c = 0x08 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            ]
        , test "0xCB 0x5A BIT 3,D (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5A
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x00 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x5A BIT 3,D (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5A
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x08 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x4B BIT 3,E (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5B
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, e = 0x00 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x5B BIT 3,E (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5B
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, e = 0x08 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x5C BIT 3,H (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5C
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6445 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x5C BIT 3,H (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5C
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x0845 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x5D BIT 3,L (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5D
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6444 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x5D BIT 3,L (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5D
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6508 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x05E BIT 3,(HL) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5E
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
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x05E BIT 3,(HL) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5E
                            |> setMemWithTime 0x6545 0x08
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
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xDD 0xCB 0x05 0x5E BIT 3, (IX + d) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime addr_1 0xCB
                            |> setMemWithTime (addr + 2) 0x05
                            |> setMemWithTime (addr + 3) 0x5E
                            |> setMemWithTime 0x6545 0x50
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | ix = 0x6540, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond

                    --mem_value =
                    --    new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                in
                Expect.equal ( addr + 4, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xDD 0xCB 0x05 0x5E BIT 3, (IX + d) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime addr_1 0xCB
                            |> setMemWithTime (addr + 2) 0x05
                            |> setMemWithTime (addr + 3) 0x5E
                            |> setMemWithTime 0x6545 0x08
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | ix = 0x6540, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond

                    --mem_value =
                    --    mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                in
                Expect.equal ( addr + 4, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x5F BIT 3,A (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5F
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x50 }
                                , flags = { flags | a = 0x30 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x5F BIT 3,A (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime addr_1 0x5F
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x51 }
                                , flags = { flags | a = 0x08 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        ]
