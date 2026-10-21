module InstructionDuration exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)


type InstructionDuration
    = FiveTStates
    | NineteenTStates
    | TwentyThreeTStates
    | NumberedTStates Int


addDuration : InstructionDuration -> CpuTimeCTime -> CpuTimeCTime
addDuration duration time =
    let
        offset =
            case duration of
                FiveTStates ->
                    5

                TwentyThreeTStates ->
                    23

                NineteenTStates ->
                    19

                NumberedTStates int ->
                    int
    in
    { time | cpu_time = time.cpu_time + offset }
