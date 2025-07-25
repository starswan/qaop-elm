module CB00Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (mem, setMem)
import Z80Flags exposing (getFlags)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x5800

        addr_plus_1 =
            addr + 1

        addr_plus_2 =
            addr + 2

        addr_minus_1 =
            addr - 1

        sp =
            0xF765

        hl =
            0xE234

        old_z80 =
            Z80.constructor.core

        old_z80env =
            old_z80.env

        z80main =
            old_z80.main

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp } |> setMem addr 0xCB, main = { z80main | hl = hl } }

        flags =
            z80.flags

        z80env =
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ describe "0xCB 0x00 RLC B"
            [ test "0x50" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_plus_1 0x00

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x50 }
                                }
                    in
                    Expect.equal ( addr + 2, 0xA0, 0xA4 ) ( new_z80.pc, new_z80.main.b, new_z80.flags |> getFlags )
            , test "0x80" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_plus_1 0x00

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x80 }
                                }
                    in
                    -- bit zero and the C flag get the bit that falls off the end
                    Expect.equal ( addr + 2, 0x01, 0x01 ) ( new_z80.pc, new_z80.main.b, new_z80.flags |> getFlags )
            , test "0xFE" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_plus_1 0x00

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0xFE }
                                }
                    in
                    Expect.equal ( addr + 2, 0xFD, 0xA9 ) ( new_z80.pc, new_z80.main.b, new_z80.flags |> getFlags )

            --        undocumented (and looks wrong)
            --, test "DD CB 00 00" <|
            --    \_ ->
            --        let
            --            new_env =
            --                z80env
            --                    |> setMem addr_minus_1 0xDD
            --                    |> setMem addr_plus_1 0x45
            --                    |> setMem addr_plus_2 0x00
            --                    |> setMem 0x6545 0x80
            --
            --            new_z80 =
            --                executeCoreInstruction z80rom
            --                    { z80
            --                        | env = new_env
            --                        , main = { z80main | ix = 0x6500, b = 0xA5 }
            --                        , pc = addr_minus_1
            --                    }
            --        in
            --        Expect.equal ( addr + 3, 0x01, 0x40 ) ( new_z80.pc, new_z80.main.b, new_z80.flags |> getFlags )
            ]
        , test "0xCB 0x01 RLC C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x01

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | c = 0x50 }
                            }
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_z80.pc, new_z80.main.c )
        , test "0xCB 0x02 RLC D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x02

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x50 }
                            }
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_z80.pc, new_z80.main.d )
        , test "0xCB 0x03 RLC E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x03

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | e = 0x50 }
                            }
                in
                Expect.equal ( addr + 2, 0xA0 ) ( new_z80.pc, new_z80.main.e )
        , test "0xCB 0x04 RLC H" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x04

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x5045, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0xA045 ) ( new_z80.pc, new_z80.main.hl )
        , test "0xCB 0x05 RLC L" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x05

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x50A0 ) ( new_z80.pc, new_z80.main.hl )
        , describe "0xCB 0x06 RLC (HL)"
            [ test "0x31" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_plus_1 0x06
                                |> setMem hl 0x31

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                }

                        mem_value =
                            new_z80.env |> mem hl new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 2, 0x62, 0x20 ) ( new_z80.pc, mem_value.value, new_z80.flags |> getFlags )
            , test "0x80" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_plus_1 0x06
                                |> setMem hl 0x80

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                }

                        mem_value =
                            new_z80.env |> mem hl new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 2, 0x01, 0x01 ) ( new_z80.pc, mem_value.value, new_z80.flags |> getFlags )
            , test "0xDD 0xCB 0x06 0x45 RLC (IX + d) 0x31" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem addr_plus_1 0xCB
                                |> setMem (addr + 2) 0x45
                                |> setMem (addr + 3) 0x06
                                |> setMem 0x6545 0x31

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, b = 0xA5 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x62, 0x20 ) ( new_z80.pc, mem_value.value, new_z80.flags |> getFlags )
            , test "0xDD 0xCB 0x06 0x45 RLC (IX + d) 0x80" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem addr_plus_1 0xCB
                                |> setMem (addr + 2) 0x45
                                |> setMem (addr + 3) 0x06
                                |> setMem 0x6545 0x80

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6545 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x01, 0x01 ) ( new_z80.pc, mem_value.value, new_z80.flags |> getFlags )
            , test "0xFD 0xCB 0x06 0x45 RLC (IY - d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem addr_plus_1 0xCB
                                |> setMem (addr + 2) 0xFF
                                |> setMem (addr + 3) 0x06
                                |> setMem 0x6500 0x31

                        new_z80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6501 }
                                }

                        mem_value =
                            new_z80.env |> mem 0x6500 new_z80.env.time z80rom
                    in
                    Expect.equal ( addr + 4, 0x62, 0x20 ) ( new_z80.pc, mem_value.value, new_z80.flags |> getFlags )
            ]
        , test "0xCB 0x07 RLC A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x07

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x30 }
                            }
                in
                Expect.equal ( addr + 2, 0x60 ) ( new_z80.pc, new_z80.flags.a )
        , test "0xCB 0x08 RRC B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x08

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, b = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.b )
        , test "0xCB 0x09 RRC C" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x09

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, c = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.c )
        , test "0xCB 0x0A RRC D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x0A

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.d )
        , test "0xCB 0x0B RRC E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x0B

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, e = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x28 ) ( new_z80.pc, new_z80.main.e )
        , test "0xCB 0x0C RRC H" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x0C

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x5045, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x2845 ) ( new_z80.pc, new_z80.main.hl )
        , test "0xCB 0x0D RRC L" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x0D

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x5050, d = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x5028 ) ( new_z80.pc, new_z80.main.hl )
        , test "0xCB 0x0E RRC (HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x0E
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
                Expect.equal ( addr + 2, 0x98 ) ( new_z80.pc, mem_value.value )
        , test "0xCB 0x0F RRC A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr_plus_1 0x0F

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
