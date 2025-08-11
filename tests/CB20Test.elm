module CB20Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (mem, setMem)
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
        [ test "0xCB 0x20 SLA B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x20

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, b = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_z80.pc, new_z80.main.b )
        , test "0xCB 0x21 SLA C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x21

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, c = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_z80.pc, new_z80.main.c )
        , test "0xCB 0x22 SLA D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x22

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_z80.pc, new_z80.main.d )
        , test "0xCB 0x23 SLA E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x23

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, e = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_z80.pc, new_z80.main.e )
        , test "0xCB 0x24 SLA H" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x24

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x5045, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0xA045 ) ( new_z80.pc, new_z80.main.hl )
        , test "0xCB 0x25 SLA L" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x25

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x50A0 ) ( new_z80.pc, new_z80.main.hl )
        , test "0xCB 0x26 SLA (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x26
                            |> setMem 0x6545 0x31

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                    mem_value =
                        new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                in
                Expect.equal ( addr + 2, 0x62 ) ( new_z80.pc, mem_value.value )
        , test "0xDD 0xCB 0x26 0x45 SLA (IX + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0x45
                            |> setMem (addr + 3) 0x26
                            |> setMem 0x6545 0x31

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | ix = 0x6500, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                    mem_value =
                        new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                in
                Expect.equal ( addr + 4, 0x62 ) ( new_z80.pc, mem_value.value )
        , test "0xFD 0xCB 0x26 0x45 SLA (IY + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xFD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0x45
                            |> setMem (addr + 3) 0x26
                            |> setMem 0x6545 0x31

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | iy = 0x6500, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                    mem_value =
                        new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                in
                Expect.equal ( addr + 4, 0x62 ) ( new_z80.pc, mem_value.value )
        , test "0xCB 0x27 SLA A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x27

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x30 }
                            }
                in
                Expect.equal ( addr + 2, 0x60 ) ( new_z80.pc, new_z80.flags.a )
        , test "0xCB 0x28 SRA B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x28

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, b = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.b )
        , test "0xCB 0x29 SRA C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x29

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, c = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.c )
        , describe "SRA D"
            [ test "0xCB 0x2A SRA D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xCB
                                |> setMem (addr + 1) 0x2A

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x50 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.d )
            , test "0xDD 0xCB d 0x2A SRA (IX + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0x45
                                |> setMem (addr + 3) 0x2A
                                |> setMem 0x6545 0x50

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, d = 0x50 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.d, mem_value.value )
            , test "0xFD 0xCB d 0x2A SRA (IY + d), D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0xFF
                                |> setMem (addr + 3) 0x2A
                                |> setMem 0x6545 0x50

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, d = 0x50 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.d, mem_value.value )
            ]
        , describe "SRA E"
            [ test "0xCB 0x2B SRA E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xCB
                                |> setMem (addr + 1) 0x2B

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x50 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.e )
            , test "0xDD 0xCB d 0x2B SRA (IX + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0x45
                                |> setMem (addr + 3) 0x2B
                                |> setMem 0x6545 0x50

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, e = 0x50 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.e, mem_value.value )
            , test "0xFD 0xCB d 0x2B SRA (IY + d), E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0xFF
                                |> setMem (addr + 3) 0x2B
                                |> setMem 0x6545 0x50

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, e = 0x50 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x28, 0x28 ) ( new_z80.pc, new_z80.main.e, mem_value.value )
            ]
        , describe "SRA H"
            [ test "0xCB 0x2C SRA H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xCB
                                |> setMem (addr + 1) 0x2C

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5045, d = 0x50 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x2845 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x2C SRA (IX + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0x45
                                |> setMem (addr + 3) 0x2C
                                |> setMem 0x6545 0x50

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x5050 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x2850, 0x28 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x2C SRA (IY + d), H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0xFF
                                |> setMem (addr + 3) 0x2C
                                |> setMem 0x6545 0x50

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5050 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x2850, 0x28 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            ]
        , describe "SRA L"
            [ test "0xCB 0x2D SRA L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xCB
                                |> setMem (addr + 1) 0x2D

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5050, d = 0x50 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x5028 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0xCB d 0x2D SRA (IX + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0x45
                                |> setMem (addr + 3) 0x2D
                                |> setMem 0x6545 0x50

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x5000 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x5028, 0x28 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            , test "0xFD 0xCB d 0x2D SRA (IY + d), L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0xFF
                                |> setMem (addr + 3) 0x2D
                                |> setMem 0x6545 0x50

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6546, hl = 0x5000 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x5028, 0x28 ) ( new_z80.pc, new_z80.main.hl, mem_value.value )
            ]
        , test "0xCB 0x02E SRA (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x2E
                            |> setMem 0x6545 0x50

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                    mem_value =
                        new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                in
                Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, mem_value.value )
        , test "0xDD 0xCB 0x2E 0x45 SRA (IX + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0x45
                            |> setMem (addr + 3) 0x2E
                            |> setMem 0x6545 0x50

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | ix = 0x6500, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                    mem_value =
                        new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                in
                Expect.equal ( addr + 4, 0x28 ) ( new_z80.pc, mem_value.value )
        , test "0xFD 0xCB 0x2E 0x45 SRA (IY + d)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xFD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0x45
                            |> setMem (addr + 3) 0x2E
                            |> setMem 0x6545 0x50

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | iy = 0x6500, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                    mem_value =
                        new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                in
                Expect.equal ( addr + 4, 0x28 ) ( new_z80.pc, mem_value.value )
        , test "0xCB 0x2F SRA A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x2F

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x30 }
                            }
                in
                Expect.equal ( addr + 2, 0x18 ) ( new_z80.pc, new_z80.flags.a )
        ]
