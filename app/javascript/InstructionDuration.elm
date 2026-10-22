module InstructionDuration exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)


type InstructionDuration
    = NumberedTStates Int


type ShortDelay
    = FiveExtraTStates
    | SevenExtraTStates


addDuration : InstructionDuration -> CpuTimeCTime -> CpuTimeCTime
addDuration duration time =
    let
        offset =
            case duration of
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
