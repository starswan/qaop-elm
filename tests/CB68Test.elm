module CB68Test exposing (..)

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
        [ describe "0xCB 0x68 BIT 5,B"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_1 0x68
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x00 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_1 0x68
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x20 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x69 BIT 5,C"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_1 0x69
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | c = 0x00 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr_1 0x69
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | c = 0x20 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x6A BIT 5,D"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6A
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x00 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6A
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x20 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x6B BIT 5,E"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6B
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | e = 0x00 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6B
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x74 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x6C BIT 5,H"
            [ test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6C
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x2045 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            , test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6C
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x45 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
            ]
        , describe "0xCB 0x6D BIT 5,L"
            [ test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6525 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            , test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6440 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
            ]
        , describe "0xCB 0x6E BIT 5,(HL)"
            [ test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6E
                                |> setMemWithTime 0x6545 0x20
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            , test "BIT 5, (IX + d) unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x05
                                |> setMemWithTime (addr + 3) 0x6E
                                |> setMemWithTime 0x6545 0x00
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6540 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 4, 0x00 ) ( new_pc, new_z80.flags.fr )
            , test "BIT 5, (IX + d) set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xCB
                                |> setMemWithTime (addr + 2) 0x05
                                |> setMemWithTime (addr + 3) 0x6E
                                |> setMemWithTime 0x6545 0x20
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6540 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 4, True ) ( new_pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x6F BIT 5,A"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6F
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x00 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime (addr + 1) 0x6F
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x20 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
            ]
        ]
