module InstructionDuration exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)


type InstructionDuration
    = FiveTStates
    | FourteenTStates
    | SeventeenTStates
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

                FourteenTStates ->
                    14

                SeventeenTStates ->
                    17

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
