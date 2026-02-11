module GroupE0Test exposing (..)

import Expect
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Mem exposing (mem, mem16)
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
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "16 bit Pop"
            [ test "POP HL (0xE1)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xE1
                                |> setMemWithTime 0xFF77 0x16
                                |> setMemWithTime 0xFF78 0x56
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal { pc = addr + 1, hl = 0x5616, sp = 0xFF79 } { sp = new_z80.env.sp, pc = new_pc, hl = new_z80.main.hl }
            , test "0xFD 0xE1 POP IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xE1
                                |> setMemWithTime sp 0x45
                                |> setMemWithTime (sp + 1) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x3445, sp + 2 ) ( new_pc, new_z80.main.ix, new_z80.env.sp )
            , test "0xFD 0xE1 POP IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xE1
                                |> setMemWithTime sp 0x45
                                |> setMemWithTime (sp + 1) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x3445, sp + 2 ) ( new_pc, new_z80.main.iy, new_z80.env.sp )
            ]
        , describe "0xE3"
            [ test "0xE3 EX (SP),HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xE3
                                |> setMemWithTime sp 0x45
                                |> setMemWithTime (sp + 1) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0xA000 }
                                }
                                |> Triple.dropSecond

                        top_lo =
                            (new_z80.env |> mem sp clock.clockTime z80rom.z80rom).value

                        top_hi =
                            (new_z80.env |> mem (sp + 1) clock.clockTime z80rom.z80rom).value
                    in
                    Expect.equal
                        { pc = new_pc, sp = new_z80.env.sp, hl = new_z80.main.hl, top_lo = top_lo, top_hi = top_hi }
                        { pc = addr + 1, sp = sp, hl = 0x3445, top_lo = 0x00, top_hi = 0xA0 }
            , test "0xDD 0xE3 EX (SP),IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xDD
                                |> setMemWithTime (addr + 1) 0xE3
                                |> setMemWithTime sp 0x45
                                |> setMemWithTime (sp + 1) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0xA000 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal
                        { pc = new_pc, sp = new_z80.env.sp, ix = new_z80.main.ix, top = (new_z80.env |> mem16 sp z80rom.z80rom clock.clockTime).value16 }
                        { pc = addr + 2, sp = sp, ix = 0x3445, top = 0xA000 }
            , test "0xFD 0xE3 EX (SP),IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0xFD
                                |> setMemWithTime (addr + 1) 0xE3
                                |> setMemWithTime sp 0x45
                                |> setMemWithTime (sp + 1) 0x34
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0xA000 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal
                        { pc = new_pc, sp = new_z80.env.sp, iy = new_z80.main.iy, top = (new_z80.env |> mem16 sp z80rom.z80rom clock.clockTime).value16 }
                        { pc = addr + 2, sp = sp, iy = 0x3445, top = 0xA000 }
            ]
        , test "0xE5 PUSH HL" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xE5
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0xA000 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal
                    { pc = new_pc, sp = new_z80.env.sp, top = (new_z80.env |> mem16 (sp - 2) z80rom.z80rom clock.clockTime).value16 }
                    { pc = addr + 1, sp = sp - 2, top = 0xA000 }
        , test "0xFD 0xE5 PUSH IY" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xFD
                            |> setMemWithTime (addr + 1) 0xE5
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0xA000 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal
                    { pc = new_pc, sp = new_z80.env.sp, top = (new_z80.env |> mem16 (sp - 2) z80rom.z80rom clock.clockTime).value16 }
                    { pc = addr + 2, sp = sp - 2, top = 0xA000 }
        , test "0xEB (EX DE, HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xEB
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5051, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                , flags = { flags | a = 0x60 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal { pc = addr + 1, hl = 0x6000, d = 0x50, e = 0x51 }
                    { pc = new_pc, hl = new_z80.main.hl, d = new_z80.main.d, e = new_z80.main.e }
        , test "0xDD 0xEB (EX DE, HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xDD
                            |> setMemWithTime (addr + 1) 0xEB
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5051, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                , flags = { flags | a = 0x60 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal { pc = addr + 2, hl = 0x6000, d = 0x50, e = 0x51 }
                    { pc = new_pc, hl = new_z80.main.hl, d = new_z80.main.d, e = new_z80.main.e }
        , test "0xFD 0xEB (EX DE, HL)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0xFD
                            |> setMemWithTime (addr + 1) 0xEB
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , main = { z80main | hl = 0x5051, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                , flags = { flags | a = 0x60 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal { pc = addr + 2, hl = 0x6000, d = 0x50, e = 0x51 }
                    { pc = new_pc, hl = new_z80.main.hl, d = new_z80.main.d, e = new_z80.main.e }
        ]
