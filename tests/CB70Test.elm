module CB70Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (setMem)
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

        old_z80 =
            Z80.constructor.core

        old_z80env =
            old_z80.env

        z80main =
            old_z80.main

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        flags =
            z80.flags

        z80env =
            z80.env |> setMem addr 0xCB

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ describe "0xCB 0x70 BIT 6,B"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x70

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x70

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x40 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x71 BIT 6,C"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x71

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | c = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x71

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | c = 0x40 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x72 BIT 6,D"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x72

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x72

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x40 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x73 BIT 6,E"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x73

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | e = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x73

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | e = 0x74 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x74 BIT 6,H"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x74

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x45 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x74

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x4045 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x75 BIT 6,L"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x75

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6400 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x75

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x76 BIT 6,(HL)"
            [ test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x76
                                |> setMem 0x6545 0x40

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            , test "BIT 6, (IX + d) unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0x05
                                |> setMem (addr + 3) 0x76
                                |> setMem 0x6545 0x00

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6540 }
                                }
                    in
                    Expect.equal ( addr + 4, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "BIT 5, (IX + d) set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0x05
                                |> setMem (addr + 3) 0x76
                                |> setMem 0x6545 0x40

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6540 }
                                }
                    in
                    Expect.equal ( addr + 4, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x77 BIT 6,A"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x77

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x77

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x40 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        ]
