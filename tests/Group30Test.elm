module Group30Test exposing (..)

import Bitwise
import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Flags exposing (c_FC, c_FZ, getFlags, setFlags)
import Z80Mem exposing (mem)
import Z80Rom


suite : Test
suite =
    -- complete 0x30 - 0x3F
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
        [ describe "0x30 JR NC, n"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x30
                                |> setMemWithTime (addr + 1) 0x05
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | ff = 0x0100 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal (addr + 2) new_pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x30
                                |> setMemWithTime (addr + 1) 0x05
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0xFF }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal (addr + 7) new_pc
            ]
        , test "0x31 - LD SP, nn" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x31
                            |> setMemWithTime (addr + 1) 0x05
                            |> setMemWithTime (addr + 2) 0x07
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 3, 0x0705 ) ( new_pc, new_z80.env.sp )
        , test "0x32 - LD (nn), A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x32
                            |> setMemWithTime (addr + 1) 0x77
                            |> setMemWithTime (addr + 2) 0x55
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond

                    mem_value =
                        new_z80.env |> mem 0x5577 clock.clockTime z80rom.z80rom
                in
                Expect.equal ( addr + 3, 0x39 ) ( new_pc, mem_value.value )
        , test "0x33 INC SP" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x33
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x8766 ) ( new_pc, new_z80.env.sp )
        , describe "16 bit indirect"
            [ test "0x34 INC (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x34
                                |> setMemWithTime 0x6545 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom.z80rom
                    in
                    Expect.equal ( addr + 1, 0x79 ) ( new_pc, mem_value.value )
            , test "0xDD 0x34 INC (IX + d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x34
                                |> setMemWithTime (addr + 2) 0x01
                                |> setMemWithTime 0x6544 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6543, hl = 0x2545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6544 clock.clockTime z80rom.z80rom
                    in
                    Expect.equal ( addr + 3, 0x79 ) ( new_pc, mem_value.value )
            , test "0xFD 0x34 INC (IY + d)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x34
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x6544 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | iy = 0x6545, hl = 0x2545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6544 clock.clockTime z80rom.z80rom
                    in
                    Expect.equal ( addr + 3, 0x79 ) ( new_pc, mem_value.value )
            , describe "0x35 DEC(HL) variants"
                [ test "0x35 DEC (HL)" <|
                    \_ ->
                        let
                            new_env =
                                z80env
                                    |> setMemWithTime addr 0x35
                                    |> setMemWithTime 0x6545 0x78
                                    |> .z80env

                            ( new_z80, new_pc ) =
                                executeCoreInstruction z80rom
                                    addr
                                    { z80
                                        | env = { new_env | sp = 0x8765 }
                                        , main = { z80main | hl = 0x6545 }
                                        , flags = { flags | a = 0x39 }
                                    }
                                    |> Triple.dropSecond

                            mem_value =
                                new_z80.env |> mem 0x6545 clock.clockTime z80rom.z80rom
                        in
                        Expect.equal ( addr + 1, 0x77, 119 ) ( new_pc, mem_value.value, new_z80.flags.fr )
                , test "0x35 DEC (HL) going to zero" <|
                    \_ ->
                        let
                            new_env =
                                z80env
                                    |> setMemWithTime addr 0x35
                                    |> setMemWithTime 0x6545 0x01
                                    |> .z80env

                            ( new_z80, new_pc ) =
                                executeCoreInstruction z80rom
                                    addr
                                    { z80
                                        | env = { new_env | sp = 0x8765 }
                                        , main = { z80main | hl = 0x6545 }
                                        , flags = { flags | a = 0x39 }
                                    }
                                    |> Triple.dropSecond

                            mem_value =
                                new_z80.env |> mem 0x6545 clock.clockTime z80rom.z80rom
                        in
                        Expect.equal ( addr + 1, 0x00, 0 ) ( new_pc, mem_value.value, new_z80.flags.fr )
                ]
            , test "0xDD 0x35 DEC (IX + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x35
                                |> setMemWithTime (addr + 2) 0xFF
                                |> setMemWithTime 0x6546 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6547 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6546 clock.clockTime z80rom.z80rom
                    in
                    Expect.equal ( addr + 3, 0x77 ) ( new_pc, mem_value.value )
            , test "0xFD 0x35 DEC (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x35
                                |> setMemWithTime (addr + 2) 0x01
                                |> setMemWithTime 0x6546 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6546 clock.clockTime z80rom.z80rom
                    in
                    Expect.equal ( addr + 3, 0x77 ) ( new_pc, mem_value.value )
            ]
        , describe "0x36 LD (HL),n variations"
            [ test "0x36 LD (HL),n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x36
                                |> setMemWithTime (addr + 1) 0xA5
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom.z80rom
                    in
                    Expect.equal ( addr + 2, 0xA5 ) ( new_pc, mem_value.value )
            , test "0xDD 0x36 LD (IX + m),n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x36
                                |> setMemWithTime (addr + 2) 0x00
                                |> setMemWithTime (addr + 3) 0xA5
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6545, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond

                        mem_value =
                            new_z80.env |> mem 0x6545 clock.clockTime z80rom.z80rom
                    in
                    Expect.equal ( addr + 4, 0xA5 ) ( new_pc, mem_value.value )
            ]
        , describe "0x37 SCF"
            [ test "zero" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x37
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = setFlags 0 z80.flags
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, c_FC ) ( new_pc, new_z80.flags |> getFlags )
            , test "with Z" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x37
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = setFlags c_FZ z80.flags
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, Bitwise.or c_FC c_FZ ) ( new_pc, new_z80.flags |> getFlags )
            , test "with all" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x37
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = setFlags 0xFE z80.flags
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0xC5 ) ( new_pc, new_z80.flags |> getFlags )
            ]
        , describe "0x38 JR C, n"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x38
                                |> setMemWithTime (addr + 1) 0x05
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | ff = 0xFF }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal (addr + 2) new_pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x38
                                |> setMemWithTime (addr + 1) 0x05
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0100 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal (addr + 7) new_pc
            ]
        , describe "0x39 ADD HL,SP variants"
            [ test "0x39 ADD HL,SP" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x39
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x4321 }
                                    , main = { z80main | hl = 0x1234 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x5555 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x39 ADD IX,SP" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x39
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x4321 }
                                    , main = { z80main | ix = 0x1234, hl = 0x4234 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x5555 ) ( new_pc, new_z80.main.ix )
            , test "0xFD 0x39 ADD IY,SP" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x39
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0x4321 }
                                    , main = { z80main | iy = 0x1234, hl = 0x4234 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x5555 ) ( new_pc, new_z80.main.iy )
            ]
        , test "0x3A LD A,(nn)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x3A
                            |> setMemWithTime (addr + 1) 0x20
                            |> setMemWithTime (addr + 2) 0x70
                            |> setMemWithTime 0x7020 0x87
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x4334 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 3, 0x87 ) ( new_pc, new_z80.flags.a )
        , test "0x3B DEC SP" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x3B
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0x8756 }
                                , main = { z80main | hl = 0x6500 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x8755 ) ( new_pc, new_z80.env.sp )
        , test "0x3C INC A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x3C
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6500 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x3A ) ( new_pc, new_z80.flags.a )
        , test "DEC A - 0x3D" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x3D
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6500 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x38 ) ( new_pc, new_z80.flags.a )
        , test "LD A,n - 0x3E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x3E
                            |> setMemWithTime (addr + 1) 0x78
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | d = 0x45, e = 0x00 }
                                , flags = { flags | a = 0x39 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x78 ) ( new_pc, new_z80.flags.a )
        , describe "0x3F CCF"
            [ test "one" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x3F
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = setFlags c_FC z80.flags
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x10 ) ( new_pc, new_z80.flags |> getFlags )
            , test "all" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x3F
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = setFlags 0xFF z80.flags
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0xD4 ) ( new_pc, new_z80.flags |> getFlags )
            ]
        ]
