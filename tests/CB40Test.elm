module CB40Test exposing (..)

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
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ test "0xCB 0x40 BIT 0,B (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x40

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, b = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x40 BIT 0,B (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x40

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, b = 0x51 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x41 BIT 0,C (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x41

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, c = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x41 BIT 0,C (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x41

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, c = 0x51 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x42 BIT 0,D (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x42

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x42 BIT 0,D (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x42

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x51 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x43 BIT 0,E (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x43

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, e = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x43 BIT 0,E (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x43

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, e = 0x51 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x44 BIT 0,H (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x44

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6445 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x44 BIT 0,H (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x44

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545 }
                            }
                in
                Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x45 BIT 0,L (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x45

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6444 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x45 BIT 0,L (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x45

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545 }
                            }
                in
                Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x046 BIT 0,(HL) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x46
                            |> setMem 0x6545 0x50

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x046 BIT 0,(HL) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x46
                            |> setMem 0x6545 0x51

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xDD 0xCB 0x05 0x46 BIT 0, (IX + d) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0x05
                            |> setMem (addr + 3) 0x46
                            |> setMem 0x6545 0x50

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | ix = 0x6540, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                    --mem_value =
                    --    mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                in
                Expect.equal ( addr + 4, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xDD 0xCB 0x05 0x46 BIT 0, (IX + d) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0x05
                            |> setMem (addr + 3) 0x46
                            |> setMem 0x6545 0x51

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | ix = 0x6540, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                    --mem_value =
                    --    mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                in
                Expect.equal ( addr + 4, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x47 BIT 0,A (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x47

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x50 }
                                , flags = { flags | a = 0x38 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        , test "0xCB 0x47 BIT 0,A (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x47

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x51 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        ]
