module InstructionDuration exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)


type InstructionDuration
    = FiveTStates
    | EighteenTStates
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

                EighteenTStates ->
                    18

                TwentyThreeTStates ->
                    23

                NineteenTStates ->
                    19

                NumberedTStates int ->
                    int
    in
    { time | cpu_time = time.cpu_time + offset }
