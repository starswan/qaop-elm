module NonCoreTests exposing (..)

import Expect
import Test exposing (..)
import Triple
import Z80 exposing (executeCoreInstruction)
import Z80CoreWithClockTime
import Z80Env exposing (setMemWithTime)
import Z80Mem exposing (mem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x8000

        sp =
            0xF765

        clock =
            Z80CoreWithClockTime.constructor

        old_z80 =
            clock.core

        old_z80env =
            old_z80.env

        z80 =
            { old_z80 | env = { old_z80env | sp = sp } }

        z80env =
            z80.env

        envwithtime =
            { z80env = z80env, time = clock.clockTime }

        z80main =
            z80.main

        flags =
            z80.flags

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "0xD2 - JP NC,nn"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            envwithtime
                                |> setMemWithTime addr 0xD2
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34

                        new_pc =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env.z80env
                                    , flags = { flags | a = 0x39, ff = 0x0100 }
                                }
                                |> Triple.third
                    in
                    Expect.equal (addr + 3) new_pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            envwithtime
                                |> setMemWithTime addr 0xD2
                                |> setMemWithTime (addr + 1) 0x05
                                |> setMemWithTime (addr + 2) 0x34

                        new_pc =
                            executeCoreInstruction z80rom
                                addr
                                { z80
                                    | env = new_env.z80env
                                    , flags = { flags | a = 0x39, ff = 0x0200 }
                                }
                                |> Triple.third
                    in
                    Expect.equal 0x3405 new_pc
            ]

        --, test "0xD9 EXX" <|
        --    \_ ->
        --        let
        --            alt =
        --                z80.alt_main
        --
        --            new_env =
        --                envwithtime
        --                    |> setMemWithTime addr 0xD9
        --                    |> setMemWithTime (addr + 1) 0x16
        --
        --            ( new_z80, new_pc ) =
        --                executeCoreInstruction z80rom addr
        --                    { z80
        --                        | env = { new_env | sp = 0xFF77 }
        --                        , alt_main = { alt | hl = 0x4040, b = 0x67, c = 0x34, d = 0x12, e = 0x81 }
        --                        , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
        --                    }
        --        in
        --        Expect.equal { pc = addr + 1, hl = 0x4040 } { pc = new_pc, hl = new_z80.main.hl }
        ]
