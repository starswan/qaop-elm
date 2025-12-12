module CB50Test exposing (..)

import Dict
import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
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
        [ test "0xCB 0x50 BIT 2,B (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x50
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
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x50 BIT 2,B (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x50
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, b = 0x04 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x51 BIT 2,C (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x51
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
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x51 BIT 2,C (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x51
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, c = 0x04 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x52 BIT 2,D (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x52
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
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x52 BIT 2,D (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x52
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x54 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x53 BIT 2,E (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x53
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
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x53 BIT 2,E (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x53
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, e = 0x54 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x54 BIT 2,H (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x54
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6045 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x54 BIT 2,H (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x54
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x55 BIT 2,L (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x55
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6440 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x55 BIT 2,L (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x55
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        , test "0xCB 0x56 BIT 2,(HL) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x56
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
        , test "0xCB 0x56 BIT 2,(HL) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x56
                            |> setMemWithTime 0x6545 0x04
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
        , test "0xDD 0xCB 0x05 0x56 BIT 2, (IX + d) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x05
                            |> setMemWithTime (addr + 3) 0x56
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
                    --    mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                in
                Expect.equal ( addr + 4, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xDD 0xCB 0x05 0x56 BIT 2, (IX + d) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xCB
                            |> setMemWithTime (addr + 2) 0x05
                            |> setMemWithTime (addr + 3) 0x56
                            |> setMemWithTime 0x6545 0x04
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
        , test "0xCB 0x57 BIT 2,A (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x57
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x50 }
                                , flags = { flags | a = 0x38 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.flags.fr )
        , test "0xCB 0x57 BIT 2,A (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xCB
                            |> setMemWithTime (addr + 1) 0x57
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x51 }
                                , flags = { flags | a = 0x04 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, True ) ( new_pc, new_z80.flags.fr /= 0 )
        ]
