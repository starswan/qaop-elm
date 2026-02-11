module Group10Test exposing (..)

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
    -- complete 0x10 - 0x1F
    let
        addr =
            30000

        clock =
            Z80CoreWithClockTime.constructor

        z80 =
            clock.core

        flags =
            z80.flags

        z80env =
            { z80env = z80.env, time = clock.clockTime }

        z80main =
            z80.main

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "DJNZ - 0x10"
            [ test "Jump" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0x10 |> setMemWithTime (addr + 1) 0x02 |> .z80env
                                    , main = { z80main | b = 0x45 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 4, 0x44 ) ( new_pc, new_z80.main.b )
            , test "Dont jump" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0x10 |> setMemWithTime (addr + 1) 0x02 |> .z80env
                                    , main = { z80main | b = 0x01 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_pc, new_z80.main.b )
            ]
        , test "0x11 LD DE,nn" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x11
                            |> setMemWithTime (addr + 1) 0x34
                            |> setMemWithTime (addr + 2) 0x45
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom addr { z80 | env = new_env } |> Triple.dropSecond
                in
                Expect.equal ( addr + 3, 0x45, 0x34 ) ( new_pc, new_z80.main.d, new_z80.main.e )
        , test "0x12 LD (DE), A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x12
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x65, e = 0x45 }
                                , flags = { flags | a = 0x38 }
                            }
                            |> Triple.dropSecond

                    mem_value =
                        new_z80.env |> mem 0x6545 clock.clockTime z80rom.z80rom
                in
                Expect.equal ( addr + 1, 0x38 ) ( new_pc, mem_value.value )
        , test "0x13 INC DE" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x13
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x65, e = 0xFF }
                                , flags = { flags | a = 0x38 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x66, 0x00 ) ( new_pc, new_z80.main.d, new_z80.main.e )
        , test "INC D 0x14" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x14
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x65, e = 0xFF }
                                , flags = { flags | a = 0x38 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x66, 0xFF ) ( new_pc, new_z80.main.d, new_z80.main.e )
        , test "0x15 DEC D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x15
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x65, e = 0xFF }
                                , flags = { flags | a = 0x38 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x64, 0xFF ) ( new_pc, new_z80.main.d, new_z80.main.e )
        , test "LD D,n - 0x16" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x16
                            |> setMemWithTime (addr + 1) 0x34
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x65, e = 0xFF }
                                , flags = { flags | a = 0x38 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x34, 0xFF ) ( new_pc, new_z80.main.d, new_z80.main.e )
        , test "RLA 0x17" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x17
                            |> setMemWithTime (addr + 1) 0x34
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x65, e = 0xFF }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x72 ) ( new_pc, new_z80.flags.a )
        , describe "0x18 JR n"
            [ test "forwards" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x18
                                |> setMemWithTime (addr + 1) 0x05
                                |> .z80env

                        new_pc =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0xFF }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.third
                    in
                    Expect.equal (addr + 7) new_pc
            , test "backwards" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x18
                                |> setMemWithTime (addr + 1) 0xFC
                                |> .z80env

                        new_pc =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0xFF }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.third
                    in
                    Expect.equal (addr - 2) new_pc
            ]
        , describe "0x19 ADD HL, DE variants"
            [ test "0x19 ADD HL, DE" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0x19 |> .z80env
                                    , main = { z80main | d = 0x12, e = 0x23, hl = 0x3445 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x4668 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x19 ADD IX, DE" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0xDD |> setMemWithTime (addr + 1) 0x19 |> .z80env
                                    , main = { z80main | ix = 0x05, d = 0x01, e = 0x02, hl = 0x3445 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0107 ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x19 ADD IY, DE" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0xFD |> setMemWithTime (addr + 1) 0x19 |> .z80env
                                    , main = { z80main | iy = 0x05, d = 0x01, e = 0x02, hl = 0x3445 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0107 ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , test "0x1A - LD A,(DE)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x1A
                            |> setMemWithTime 0x4546 0x78
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x45, e = 0x46 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x78 ) ( new_pc, new_z80.flags.a )
        , test "0x1B DEC DE" <|
            \_ ->
                let
                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = z80env |> setMemWithTime addr 0x1B |> .z80env
                                , main = { z80main | d = 0x45, e = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x44, 0xFF ) ( new_pc, new_z80.main.d, new_z80.main.e )
        , test "INC E - 0x1C" <|
            \_ ->
                let
                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = z80env |> setMemWithTime addr 0x1C |> .z80env
                                , main = { z80main | d = 0x45, e = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x45, 0x01 ) ( new_pc, new_z80.main.d, new_z80.main.e )
        , test "DEC E - 0x1D" <|
            \_ ->
                let
                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = z80env |> setMemWithTime addr 0x1D |> .z80env
                                , main = { z80main | d = 0x45, e = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x45, 0xFF ) ( new_pc, new_z80.main.d, new_z80.main.e )
        , test "LD E,n - 0x1E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x1E
                            |> setMemWithTime (addr + 1) 0x78
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x45, e = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x78 ) ( new_pc, new_z80.main.e )
        , test "RRA 0x1F" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x1F
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x65, e = 0xFF }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x1C ) ( new_pc, new_z80.flags.a )
        ]
