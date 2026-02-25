module Group00Test exposing (..)

import CpuTimeCTime exposing (InstructionDuration(..))
import Expect exposing (Expectation)
import PCIncrement exposing (PCIncrement(..))
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Flags exposing (getFlags)
import Z80Mem exposing (mem)
import Z80OpCode exposing (lengthAndDuration)
import Z80Rom


suite : Test
suite =
    -- complete - 0x00 - 0x0F
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
        [ describe "0x00 NOP"
            [ test "execute NOP" <|
                \_ ->
                    let
                        ( clockTime, new_pc ) =
                            { z80 | env = z80env |> setMemWithTime addr 0x00 |> .z80env }
                                |> Z80.executeCoreInstruction z80rom addr
                                |> Triple.dropFirst
                    in
                    Expect.equal ( addr + 1, 4 ) ( new_pc, clockTime.cpu_time - clock.clockTime.cpu_time )
            , test "length NOP" <|
                \_ ->
                    let
                        z80inc =
                            { z80 | env = z80env |> setMemWithTime addr 0x00 |> .z80env }
                    in
                    lengthAndDuration addr z80rom.z80rom z80inc.env |> Maybe.map (\d -> d |> Triple.dropThird) |> Expect.equal (Just ( IncrementByOne, FourTStates ))
            ]
        , describe "0x01 LD BC,nn"
            [ test "Execute 0x01" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x01
                                |> setMemWithTime (addr + 1) 0x34
                                |> setMemWithTime (addr + 2) 0x45
                                |> .z80env

                        ( new_z80, new_pc ) =
                            { z80 | env = new_env } |> Z80.executeCoreInstruction z80rom addr |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 3, 0x45, 0x34 ) ( new_pc, new_z80.main.b, new_z80.main.c )
            ]
        , describe "0x02 LD (BC), A"
            [ test "execute LD (BC), A" <|
                \_ ->
                    let
                        z80inc =
                            { z80
                                | env = z80env |> setMemWithTime addr 0x02 |> .z80env
                                , main = { z80main | b = 0x45, c = 0x34 }
                                , flags = { flags | a = 0x27 }
                            }

                        ( new_z80, new_pc ) =
                            z80inc |> Z80.executeCoreInstruction z80rom addr |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x27 ) ( new_pc, new_z80.env |> mem 0x4534 clock.clockTime z80rom.z80rom |> .value )
            , test "length LD (BC),A" <|
                \_ ->
                    let
                        z80inc =
                            z80env
                                |> setMemWithTime addr 0x02
                                |> .z80env
                    in
                    lengthAndDuration addr z80rom.z80rom z80inc |> Maybe.map (\d -> d |> Triple.dropThird) |> Expect.equal (Just ( IncrementByOne, SevenTStates ))
            ]
        , describe "0x03 INC BC"
            [ test "execute INC BC" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0x03 |> .z80env
                                    , main = { z80main | b = 0x45, c = 0xFF }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x46, 0x00 ) ( new_pc, new_z80.main.b, new_z80.main.c )
            , test "length INC BC" <|
                \_ ->
                    let
                        z80inc =
                            z80env
                                |> setMemWithTime addr 0x03
                                |> .z80env
                    in
                    lengthAndDuration addr z80rom.z80rom z80inc |> Maybe.map (\d -> d |> Triple.dropThird) |> Expect.equal (Just ( IncrementByOne, SixTStates ))
            ]
        , test "0x04 INC B" <|
            \_ ->
                let
                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = z80env |> setMemWithTime addr 0x04 |> .z80env
                                , main = { z80main | b = 0x45 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x46 ) ( new_pc, new_z80.main.b )
        , test "0x05 DEC B" <|
            \_ ->
                let
                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = z80env |> setMemWithTime addr 0x05 |> .z80env
                                , main = { z80main | b = 0x45 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x44 ) ( new_pc, new_z80.main.b )
        , describe "0x06 LD B,n"
            [ test "execute LD B,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x06
                                |> setMemWithTime (addr + 1) 0x78
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom addr { z80 | env = new_env } |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x78 ) ( new_pc, new_z80.main.b )
            , test "length LD B,n" <|
                \_ ->
                    let
                        z80inc =
                            z80env
                                |> setMemWithTime addr 0x06
                                |> setMemWithTime (addr + 1) 0x78
                                |> .z80env
                    in
                    lengthAndDuration addr z80rom.z80rom z80inc |> Maybe.map (\d -> d |> Triple.dropThird) |> Expect.equal (Just ( IncrementByTwo, SevenTStates ))
            ]
        , describe "RLCA 0x07"
            [ test "with carry" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x07
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x87 }
                                }
                                |> Triple.dropSecond
                    in
                    -- This is RLCA - bit 7 goes into bit 0 and carry flag
                    Expect.equal ( addr + 1, 0x0F, 0x49 ) ( new_pc, new_z80.flags.a, new_z80.flags |> getFlags )
            , test "without carry" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMemWithTime addr 0x07
                                |> .z80env

                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x47 }
                                }
                                |> Triple.dropSecond
                    in
                    -- This is RLCA - bit 7 goes into bit 0 and carry flag
                    Expect.equal ( addr + 1, 0x8E, 0x48 ) ( new_pc, new_z80.flags.a, new_z80.flags |> getFlags )
            , test "length RLCA" <|
                \_ ->
                    let
                        z80inc =
                            z80env
                                |> setMemWithTime addr 0x07
                                |> .z80env
                    in
                    lengthAndDuration addr z80rom.z80rom z80inc |> Maybe.map (\d -> d |> Triple.dropThird) |> Expect.equal (Just ( IncrementByOne, FourTStates ))
            ]

        --, describe "EX AF,AF'"
        --    [ test "0x08" <|
        --        \_ ->
        --            let
        --                ( new_z80, new_pc ) =
        --                    executeSingleInstruction z80rom
        --                        { z80
        --                            | env = z80env |> setMem addr 0x08
        --                            , flags = { flags | a = 0x87 }
        --                        }
        --            in
        --            Expect.equal ( addr + 1, 0x87, z80.alt_flags ) ( new_pc, new_z80.alt_flags.a, new_z80.flags )
        --    ]
        , describe "ADD HL, 16-bit"
            [ test "0x09 ADD HL, BC" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0x09 |> .z80env
                                    , main = { z80main | ix = 0x27, b = 0x01, c = 0x02, hl = 0x0304 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 1, 0x0406, 0x27 ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xDD 0x09 ADD IX, BC" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0xDD |> setMemWithTime (addr + 1) 0x09 |> .z80env
                                    , main = { z80main | ix = 0x05, b = 0x01, c = 0x02, hl = 0x3445 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0107 ) ( new_pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x09 ADD IY, BC" <|
                \_ ->
                    let
                        ( new_z80, new_pc ) =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = z80env |> setMemWithTime addr 0xFD |> setMemWithTime (addr + 1) 0x09 |> .z80env
                                    , main = { z80main | iy = 0x05, b = 0x01, c = 0x02, hl = 0x3445 }
                                }
                                |> Triple.dropSecond
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0107 ) ( new_pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , test "0x0A - LD A,(BC)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x0A
                            |> setMemWithTime 0x4546 0x78
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x45, c = 0x46 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x78 ) ( new_pc, new_z80.flags.a )
        , test "0x0B DEC BC" <|
            \_ ->
                let
                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = z80env |> setMemWithTime addr 0x0B |> .z80env
                                , main = { z80main | b = 0x45, c = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x44, 0xFF ) ( new_pc, new_z80.main.b, new_z80.main.c )
        , test "INC C - 0x0C" <|
            \_ ->
                let
                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = z80env |> setMemWithTime addr 0x0C |> .z80env
                                , main = { z80main | b = 0x45, c = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x45, 0x01 ) ( new_pc, new_z80.main.b, new_z80.main.c )
        , test "DEC C - 0x0D" <|
            \_ ->
                let
                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = z80env |> setMemWithTime addr 0x0D |> .z80env
                                , main = { z80main | b = 0x45, c = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x45, 0xFF ) ( new_pc, new_z80.main.b, new_z80.main.c )
        , test "LD C,n - 0x0E" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemWithTime addr 0x0E
                            |> setMemWithTime (addr + 1) 0x78
                            |> .z80env

                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x45, c = 0x00 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 2, 0x45, 0x78 ) ( new_pc, new_z80.main.b, new_z80.main.c )
        , test "RRCA - 0x0F" <|
            \_ ->
                let
                    ( new_z80, new_pc ) =
                        executeCoreInstruction z80rom
                            addr
                            { z80
                                | env = z80env |> setMemWithTime addr 0x0F |> .z80env
                                , flags = { flags | a = 0x80 }
                            }
                            |> Triple.dropSecond
                in
                Expect.equal ( addr + 1, 0x40 ) ( new_pc, new_z80.flags.a )
        ]
