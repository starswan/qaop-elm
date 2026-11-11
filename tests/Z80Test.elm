module Z80Test exposing (..)

import CpuTimeCTime exposing (reset_cpu_time)
import Expect
import Test exposing (..)
import Utils exposing (toHexString)
import Z80CoreWithClockTime
import Z80Env exposing (setMem, setMem16)
import Z80Mem exposing (getMem8, mem16)
import Z80Rom


suite : Test
suite =
    let
        clock =
            Z80CoreWithClockTime.constructor

        z80 =
            clock.core

        z80env =
            z80.env

        envwithtime =
            { z80env = z80env, time = reset_cpu_time }

        z80rom =
            Z80Rom.emptyROM
    in
    describe "memory access"
        [ describe "8 bit writes"
            [ test "write to upper RAM" <|
                \_ ->
                    let
                        ( newEnv, newTime ) =
                            z80env |> setMem 0xFFFE 0xDD envwithtime.time

                        ( newEnv2, newTime2 ) =
                            newEnv |> setMem 0xFFFF 0xCC newTime

                        value2 =
                            newEnv2 |> mem16 0xFFFE z80rom newTime2 |> .value16

                        value3 =
                            newEnv2 |> mem16 0xFFFF z80rom newTime2 |> .value16
                    in
                    Expect.equal ( 0xCCDD, 0xCC ) ( value2, value3 )
            , test "write to  bank1" <|
                \_ ->
                    let
                        ( newEnv, newTime ) =
                            z80env |> setMem 0x4000 0xDD envwithtime.time

                        ( newEnv2, newTime2 ) =
                            newEnv |> setMem 0x4001 0xCC newTime

                        value2 =
                            newEnv2 |> mem16 0x3FFF z80rom newTime2 |> .value16

                        value3 =
                            newEnv2 |> mem16 0x4000 z80rom newTime2 |> .value16
                    in
                    Expect.equal ( "0xDD00", "0xCCDD" ) ( value2 |> toHexString, value3 |> toHexString )
            , test "write to bank2" <|
                \_ ->
                    let
                        ( newEnv, newTime ) =
                            z80env |> setMem 0x8000 0xDD envwithtime.time

                        ( newEnv2, newTime2 ) =
                            newEnv |> setMem 0x8001 0xCC newTime

                        value2 =
                            newEnv2 |> mem16 0x7FFF z80rom newTime2 |> .value16

                        value3 =
                            newEnv2 |> mem16 0x8000 z80rom newTime2 |> .value16
                    in
                    Expect.equal ( "0xDD00", "0xCCDD" ) ( value2 |> toHexString, value3 |> toHexString )
            , test "write to bank3" <|
                \_ ->
                    let
                        ( newEnv, newTime ) =
                            z80env |> setMem 0xC000 0xDD envwithtime.time

                        ( newEnv2, newTime2 ) =
                            newEnv |> setMem 0xC001 0xCC newTime

                        value2 =
                            newEnv2 |> mem16 0xBFFF z80rom newTime2 |> .value16

                        value3 =
                            newEnv2 |> mem16 0xC000 z80rom newTime2 |> .value16
                    in
                    Expect.equal ( "0xDD00", "0xCCDD" ) ( value2 |> toHexString, value3 |> toHexString )
            ]
        , describe "16 bit writes"
            [ test "write to ROM and RAM" <|
                \_ ->
                    let
                        ( newEnv, newTime ) =
                            z80env |> setMem16 0x3FFF 0xCCDD envwithtime.time

                        value =
                            newEnv |> getMem8 0x3FFF newTime z80rom |> Tuple.first

                        valu2 =
                            newEnv |> getMem8 0x4000 newTime z80rom |> Tuple.first
                    in
                    Expect.equal ( 0x00, 0xCC ) ( value, valu2 )
            , test "write to banks 1 AND 2" <|
                \_ ->
                    let
                        ( newEnv, newTime ) =
                            z80env |> setMem16 0x7FFF 0xCCDD envwithtime.time

                        value =
                            newEnv |> getMem8 0x7FFF newTime z80rom |> Tuple.first

                        valu2 =
                            newEnv |> getMem8 0x8000 newTime z80rom |> Tuple.first
                    in
                    Expect.equal ( 0xDD, 0xCC ) ( value, valu2 )
            , test "write to banks 2 AND 3" <|
                \_ ->
                    let
                        ( newEnv, newTime ) =
                            z80env |> setMem16 0xBFFF 0xCCDD envwithtime.time

                        value =
                            newEnv |> getMem8 0xBFFF newTime z80rom |> Tuple.first

                        valu2 =
                            newEnv |> getMem8 0xC000 newTime z80rom |> Tuple.first
                    in
                    Expect.equal ( 0xDD, 0xCC ) ( value, valu2 )
            , test "write to banks 3 and ROM" <|
                \_ ->
                    let
                        ( newEnv, newTime ) =
                            z80env |> setMem16 0xFFFF 0xCCDD envwithtime.time

                        value =
                            newEnv |> getMem8 0xFFFF newTime z80rom |> Tuple.first

                        valu2 =
                            newEnv |> getMem8 0x00 newTime z80rom |> Tuple.first
                    in
                    Expect.equal ( 0xDD, 0x00 ) ( value, valu2 )
            ]
        ]
