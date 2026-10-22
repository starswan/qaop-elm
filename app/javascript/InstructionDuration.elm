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
    | SeventeenTStates
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
                NineteenTStates ->
                    19

                NumberedTStates int ->
                    int
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
