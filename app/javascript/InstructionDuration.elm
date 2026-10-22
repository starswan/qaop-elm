module InstructionDuration exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)


type InstructionDuration
    = FourTStates
    | FiveTStates
    | SixTStates
    | SevenTStates
    | EightTStates
    | NineTStates
    | TenTStates
    | ElevenTStates
    | TwelveTStates
    | ThirteenTStates
    | FourteenTStates
    | FifteenTStates
    | SixteenTStates
    | EighteenTStates
    | NineteenTStates
    | TwentyTStates
    | TwentyThreeTStates


type ShortDelay
    = FiveExtraTStates
    | SevenExtraTStates


addDuration : InstructionDuration -> CpuTimeCTime -> CpuTimeCTime
addDuration duration time =
    let
        offset =
            case duration of
                --ZeroTStates ->
                --    value
                FourTStates ->
                    4

                FiveTStates ->
                    5

                SixTStates ->
                    6

                SevenTStates ->
                    7

                EightTStates ->
                    8

                NineTStates ->
                    9

                TenTStates ->
                    10

                ElevenTStates ->
                    11

                TwelveTStates ->
                    12

                ThirteenTStates ->
                    13

                FourteenTStates ->
                    14

                FifteenTStates ->
                    15

                SixteenTStates ->
                    16

                EighteenTStates ->
                    18

                TwentyTStates ->
                    20

                TwentyThreeTStates ->
                    23

                NineteenTStates ->
                    19
    in
    { time | cpu_time = time.cpu_time + offset }


addExtraCpuTime : ShortDelay -> CpuTimeCTime -> CpuTimeCTime
addExtraCpuTime value cpuTime =
    let
        offset =
            case value of
                FiveExtraTStates ->
                    5

                SevenExtraTStates ->
                    7
    in
    { cpuTime | cpu_time = cpuTime.cpu_time + offset }
