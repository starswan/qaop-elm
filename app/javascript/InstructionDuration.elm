module InstructionDuration exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)


type InstructionDuration
    = NineteenTStates
    | NumberedTStates Int


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
addExtraCpuTime value z80env =
    let
        offset =
            case value of
                FiveExtraTStates ->
                    5

                SevenExtraTStates ->
                    7
    in
    { z80env | cpu_time = z80env.cpu_time + offset }
