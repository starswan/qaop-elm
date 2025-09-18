module CBC0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeCoreInstruction)
import Z80Env exposing (mem, setMemIgnoringTime)
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

        z80env =
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ test "0xCB C0 SET 0,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xCB
                            |> setMemIgnoringTime (addr + 1) 0xC0

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x00 }
                            }
                in
                Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.main.b )
        , test "0xDD 0xCB nn 0xC6 SET 0, (IX + n)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xDD
                            |> setMemIgnoringTime (addr + 1) 0xCB
                            |> setMemIgnoringTime (addr + 2) 0x06
                            |> setMemIgnoringTime (addr + 3) 0xC6
                            |> setMemIgnoringTime 0xA086 0x10

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | ix = 0xA080 }
                            }

                    mem_value =
                        new_z80.env |> mem 0xA086 new_z80.env.time z80rom
                in
                Expect.equal ( addr + 4, 0x11 ) ( new_z80.pc, mem_value.value )
        , test "0xCB C8 SET 1,B" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMemIgnoringTime addr 0xCB
                            |> setMemIgnoringTime (addr + 1) 0xC8

                    new_z80 =
                        executeCoreInstruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | b = 0x00 }
                            }
                in
                Expect.equal ( addr + 2, 0x02 ) ( new_z80.pc, new_z80.main.b )
        ]
