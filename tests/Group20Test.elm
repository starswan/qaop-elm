module Group20Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMem16WithTime, setMemWithTime)
import Z80Mem exposing (mem)
import Z80Rom


suite : Test
suite =
    -- complete 0x20 - 0x2F
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
        [ describe "0x20 - JR NZ,n"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x20
                                |> setMemWithTime (addr + 1) 0x05
                                |> .z80env

                        ( clockTime, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropFirst
                    in
                    Expect.equal ( addr + 2, 7 ) ( new_pc, clockTime.cpu_time - clock.clockTime.cpu_time )
            , test "forwards" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x20
                                |> setMemWithTime (addr + 1) 0x05
                                |> .z80env

                        ( clockTime, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                                |> Triple.dropFirst
                    in
                    Expect.equal ( addr + 7, 12 ) ( new_pc, clockTime.cpu_time - clock.clockTime.cpu_time )
            , test "backwards" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x20
                                |> setMemWithTime (addr + 1) 0xFB
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal (addr - 3) new_pc
            ]
        , describe "0x21 LD HL, nn versions"
            [ test "0x21 - LD HL, nn" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x21
                                |> setMemWithTime (addr + 1) 0xC6
                                |> setMemWithTime (addr + 2) 0x15
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
                    Expect.equal ( addr + 3, 0x15C6 ) ( new_pc, new_z80.main.hl )
            , test "0xFD 0x21 - LD IX, nn" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x21
                                |> setMemWithTime (addr + 2) 0x05
                                |> setMemWithTime (addr + 3) 0x07
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
                    Expect.equal ( addr + 4, 0x0705 ) ( new_pc, new_z80.main.ix )
            , test "0xFD 0x21 - LD IY, nn" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x21
                                |> setMemWithTime (addr + 2) 0x05
                                |> setMemWithTime (addr + 3) 0x07
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
                    Expect.equal ( addr + 4, 0x0705 ) ( new_pc, new_z80.main.iy )
            ]
        , describe "0x22 LD (nn), HL variants"
            [ test "0x22 LD (nn), HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x22
                                |> setMemWithTime (addr + 1) 0x77
                                |> setMemWithTime (addr + 2) 0x55
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x4D8F }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond

                        lo_value =
                            new_z80.env |> mem 0x5577 clock.clockTime z80rom

                        high_value =
                            new_z80.env |> mem 0x5578 clock.clockTime z80rom
                    in
                    ( new_pc, lo_value.value, high_value.value ) |> Expect.equal ( addr + 3, 0x8F, 0x4D )
            , test "0xDD 22 LD (nn), IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x22
                                |> setMemWithTime (addr + 2) 0x77
                                |> setMemWithTime (addr + 3) 0x55
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x4D8F }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond

                        lo_value =
                            new_z80.env |> mem 0x5577 clock.clockTime z80rom

                        high_value =
                            new_z80.env |> mem 0x5578 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x8F, 0x4D ) ( new_pc, lo_value.value, high_value.value )
            , test "0xFD 22 LD (nn), IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x22
                                |> setMemWithTime (addr + 2) 0x77
                                |> setMemWithTime (addr + 3) 0x55
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x4D8F }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond

                        lo_value =
                            new_z80.env |> mem 0x5577 clock.clockTime z80rom

                        high_value =
                            new_z80.env |> mem 0x5578 clock.clockTime z80rom
                    in
                    Expect.equal ( addr + 4, 0x8F, 0x4D ) ( new_pc, lo_value.value, high_value.value )
            ]
        , describe "0x23 INC HL variants"
            [ test "0x23 INC HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x23
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
                    in
                    Expect.equal ( addr + 1, 0x6546 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x23 INC IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x23
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0xFFFF, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6545, 0 ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x23 INC IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x23
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0xFFFE, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6545, 0xFFFF ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , describe "0x24 INC H variants"
            [ test "0x24 INC H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x24
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x6645, 0x6545 ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            , test "0xDD 0x24 INC IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x24
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6545, 0x6645 ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x24 INC IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x24
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6545, 0x6645 ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , describe "0x25 DEC H variants"
            [ test "0x25 DEC H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x25
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
                    in
                    Expect.equal ( addr + 1, 0x6445 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x25 DEC IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x25
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x45, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6545, 0xFF45 ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x25 DEC IYH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x25
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x45, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6545, 0xFF45 ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , describe "0x26 LD H,n variants"
            [ test "0x26 - LD H,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x26
                                |> setMemWithTime (addr + 1) 0x05
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
                    Expect.equal ( addr + 2, 0x0545 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x26 - LD IXH,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x26
                                |> setMemWithTime (addr + 2) 0x05
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6545, hl = 0x6545 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0x6545, 0x0545 ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x26 - LD IYH,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x26
                                |> setMemWithTime (addr + 2) 0x05
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, hl = 0x6545 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0x6545, 0x0545 ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , test "0x27 - DAA" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x27
                            |> setMemWithTime (addr + 1) 0x05
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
                Expect.equal ( addr + 1, 0x39 ) ( new_pc, new_z80.flags.a )
        , describe "0x28 JR Z, n"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x28
                                |> setMemWithTime (addr + 1) 0x05
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | fr = 1 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal (addr + 2) new_pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x28
                                |> setMemWithTime (addr + 1) 0x05
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 0 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal (addr + 7) new_pc
            ]
        , describe "ADD HL, 16-bit"
            [ test "0x29 ADD HL,HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x29
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
                    Expect.equal ( addr + 1, 0x8668 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x29 ADD IX, IX" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0xDD |> setMemWithTime (addr + 1) 0x29 |> .z80env
                                    , main = { z80main | ix = 0x05, b = 0x01, c = 0x02, hl = 0x3445 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0A ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x29 ADD IY, IY" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0xFD |> setMemWithTime (addr + 1) 0x29 |> .z80env
                                    , main = { z80main | iy = 0x05, b = 0x01, c = 0x02, hl = 0x3445 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0A ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , describe "load reg indirect"
            [ test "0x2A LD HL,(nn)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x2A
                                |> setMemWithTime (addr + 1) 0x34
                                |> setMemWithTime (addr + 2) 0x54
                                |> setMem16WithTime 0x5434 0x8723
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
                    Expect.equal ( addr + 3, 0x8723 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x2A LD IX,(nn)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x2A
                                |> setMemWithTime (addr + 2) 0x34
                                |> setMemWithTime (addr + 3) 0x54
                                |> setMem16WithTime 0x5434 0x8723
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x4334, hl = 0x4334 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 4, 0x8723 ) ( new_pc, new_z80.main.ix )
            , test "0xFD 0x2A LD IY,(nn)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x2A
                                |> setMemWithTime (addr + 2) 0x34
                                |> setMemWithTime (addr + 3) 0x54
                                |> setMem16WithTime 0x5434 0x8723
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x4334, hl = 0x4334 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 4, 0x8723 ) ( new_pc, new_z80.main.iy )
            ]
        , describe "DEC 16 bit"
            [ test "0x2B DEC HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x2B
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
                    Expect.equal ( addr + 1, 0x64FF ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x2B DEC IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x2B
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6500, 0xFFFF ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x2B DEC IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x2B
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6500, 0xFFFF ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , describe "0x2C INC 8 bit"
            [ test "0x2C INC L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x2C
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
                    Expect.equal ( addr + 1, 0x6501 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x2C INC IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x2C
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6500, 0x6501 ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x2C INC IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x2C
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x6500, 0x6501 ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , describe "0x2D DEC L varaints"
            [ test "0x2D DEC L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x2D
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
                    Expect.equal ( addr + 1, 0x65FF ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x2D DEC IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x2D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x65FF, 0x6500 ) ( new_pc, new_z80.main.ix, new_z80.main.hl )
            , test "0xFD 0x2D DEC IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x2D
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x65FF, 0x6500 ) ( new_pc, new_z80.main.iy, new_z80.main.hl )
            ]
        , describe "LD 8-bit,n"
            [ test "0x2E LD L,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x2E
                                |> setMemWithTime (addr + 1) 0x34
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
                    Expect.equal ( addr + 2, 0x6534 ) ( new_pc, new_z80.main.hl )
            , test "0xDD 0x2E LD IXL,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0x2E
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0x6534, 0x6500 ) ( new_pc, new_z80.main.ix, new_z80.main.hl )
            , test "0xFD 0x2E LD IYL,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0x2E
                                |> setMemWithTime (addr + 2) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0x6534, 0x6500 ) ( new_pc, new_z80.main.iy, new_z80.main.hl )
            ]
        , test "0x2F CPL" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x2F
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
                Expect.equal ( addr + 1, 0xC6 ) ( new_pc, new_z80.flags.a )
        ]
