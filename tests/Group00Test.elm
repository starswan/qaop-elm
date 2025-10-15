module Group00Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (mem, setMemWithTime)
import Z80Flags exposing (getFlags)
import Z80Rom


suite : Test
suite =
    -- complete - 0x00 - 0x0F
    let
        addr =
            30000

        clock =
            Z80CoreWithClockTime.constructor

        old_z80 =
            clock.core

        z80 =
            { old_z80 | pc = addr }

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
        [ test "0x00" <|
            \_ ->
                let
                    ( z80inc, clockTime ) =
                        { z80 | env = z80env |> setMemWithTime addr 0x00 |> .z80env } |> Z80.executeCoreInstruction z80rom
                in
                Expect.equal ( addr + 1, 4 ) ( z80inc.pc, clockTime.cpu_time - clock.clockTime.cpu_time )
        , test "0x01 LD BC,nn" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x01
                            |> setMemWithTime (addr + 1) 0x34
                            |> setMemWithTime (addr + 2) 0x45
                            |> .z80env

                    z80_after_01 =
                        { z80 | env = new_env } |> Z80.executeCoreInstruction z80rom |> Tuple.first
                in
                Expect.equal ( addr + 3, 0x45, 0x34 ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
        , test "0x02 LD (BC), A" <|
            \_ ->
                let
                    z80inc =
                        { z80
                            | env = z80env |> setMemWithTime addr 0x02 |> .z80env
                            , main = { z80main | b = 0x45, c = 0x34 }
                            , flags = { flags | a = 0x27 }
                        }

                    z80_after_01 =
                        z80inc |> Z80.executeCoreInstruction z80rom |> Tuple.first

                    mem_value =
                        z80_after_01.env |> mem 0x4534 clock.clockTime z80rom
                in
                Expect.equal ( addr + 1, 0x27 ) ( z80_after_01.pc, mem_value.value )
        , test "0x03 INC BC" <|
            \_ ->
                let
                    z80_after_01 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = z80env |> setMemWithTime addr 0x03 |> .z80env
                                , main = { z80main | b = 0x45, c = 0xFF }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 1, 0x46, 0x00 ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
        , test "0x04 INC B" <|
            \_ ->
                let
                    z80_after_01 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = z80env |> setMemWithTime addr 0x04 |> .z80env
                                , main = { z80main | b = 0x45 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 1, 0x46 ) ( z80_after_01.pc, z80_after_01.main.b )
        , test "0x05 DEC B" <|
            \_ ->
                let
                    z80_after_01 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = z80env |> setMemWithTime addr 0x05 |> .z80env
                                , main = { z80main | b = 0x45 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 1, 0x44 ) ( z80_after_01.pc, z80_after_01.main.b )
        , test "0x06 LD B,n" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x06
                            |> setMemWithTime (addr + 1) 0x78
                            |> .z80env

                    z80_after_01 =
                        executeCoreInstruction z80rom { z80 | env = new_env } |> Tuple.first
                in
                Expect.equal ( addr + 2, 0x78 ) ( z80_after_01.pc, z80_after_01.main.b )
        , describe "RLCA 0x07"
            [ test "with carry" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x07
                                |> .z80env

                        newZ80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x87 }
                                }
                                |> Tuple.first
                    in
                    -- This is RLCA - bit 7 goes into bit 0 and carry flag
                    Expect.equal ( addr + 1, 0x0F, 0x49 ) ( newZ80.pc, newZ80.flags.a, newZ80.flags |> getFlags )
            , test "without carry" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x07
                                |> .z80env

                        newZ80 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x47 }
                                }
                                |> Tuple.first
                    in
                    -- This is RLCA - bit 7 goes into bit 0 and carry flag
                    Expect.equal ( addr + 1, 0x8E, 0x48 ) ( newZ80.pc, newZ80.flags.a, newZ80.flags |> getFlags )
            ]

        --, describe "EX AF,AF'"
        --    [ test "0x08" <|
        --        \_ ->
        --            let
        --                z80_after_01 =
        --                    executeSingleInstruction z80rom
        --                        { z80
        --                            | env = z80env |> setMem addr 0x08
        --                            , flags = { flags | a = 0x87 }
        --                        }
        --            in
        --            Expect.equal ( addr + 1, 0x87, z80.alt_flags ) ( z80_after_01.pc, z80_after_01.alt_flags.a, z80_after_01.flags )
        --    ]
        , describe "ADD HL, 16-bit"
            [ test "0x09 ADD HL, BC" <|
                \_ ->
                    let
                        z80_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = z80env |> setMemWithTime addr 0x09 |> .z80env
                                    , main = { z80main | ix = 0x27, b = 0x01, c = 0x02, hl = 0x0304 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 1, 0x0406, 0x27 ) ( z80_01.pc, z80_01.main.hl, z80_01.main.ix )
            , test "0xDD 0x09 ADD IX, BC" <|
                \_ ->
                    let
                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = z80env |> setMemWithTime addr 0xDD |> setMemWithTime (addr + 1) 0x09 |> .z80env
                                    , main = { z80main | ix = 0x05, b = 0x01, c = 0x02, hl = 0x3445 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0107 ) ( z80_after_01.pc, z80_after_01.main.hl, z80_after_01.main.ix )
            , test "0xFD 0x09 ADD IY, BC" <|
                \_ ->
                    let
                        z80_after_01 =
                            executeCoreInstruction z80rom
                                { z80
                                    | env = z80env |> setMemWithTime addr 0xFD |> setMemWithTime (addr + 1) 0x09 |> .z80env
                                    , main = { z80main | iy = 0x05, b = 0x01, c = 0x02, hl = 0x3445 }
                                }
                                |> Tuple.first
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0107 ) ( z80_after_01.pc, z80_after_01.main.hl, z80_after_01.main.iy )
            ]
        , test "0x0A - LD A,(BC)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x0A
                            |> setMemWithTime 0x4546 0x78
                            |> .z80env

                    z80_after_01 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x45, c = 0x46 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 1, 0x78 ) ( z80_after_01.pc, z80_after_01.flags.a )
        , test "0x0B DEC BC" <|
            \_ ->
                let
                    z80_after_01 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = z80env |> setMemWithTime addr 0x0B |> .z80env
                                , main = { z80main | b = 0x45, c = 0x00 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 1, 0x44, 0xFF ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
        , test "INC C - 0x0C" <|
            \_ ->
                let
                    z80_after_01 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = z80env |> setMemWithTime addr 0x0C |> .z80env
                                , main = { z80main | b = 0x45, c = 0x00 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 1, 0x45, 0x01 ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
        , test "DEC C - 0x0D" <|
            \_ ->
                let
                    z80_after_01 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = z80env |> setMemWithTime addr 0x0D |> .z80env
                                , main = { z80main | b = 0x45, c = 0x00 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 1, 0x45, 0xFF ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
        , test "LD C,n - 0x0E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x0E
                            |> setMemWithTime (addr + 1) 0x78
                            |> .z80env

                    z80_after_01 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x45, c = 0x00 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 2, 0x45, 0x78 ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
        , test "RRCA - 0x0F" <|
            \_ ->
                let
                    z80_after_01 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = z80env |> setMemWithTime addr 0x0F |> .z80env
                                , flags = { flags | a = 0x80 }
                            }
                            |> Tuple.first
                in
                Expect.equal ( addr + 1, 0x40 ) ( z80_after_01.pc, z80_after_01.flags.a )
        ]
