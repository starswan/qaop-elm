module InstructionDuration exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)


type InstructionDuration
    = FiveTStates
    | NineTStates
    | FourteenTStates
    | SeventeenTStates
    | EighteenTStates
    | NineteenTStates
    | TwentyTStates
    | TwentyThreeTStates
    | NumberedTStates Int


addDuration : InstructionDuration -> CpuTimeCTime -> CpuTimeCTime
addDuration duration time =
    let
        offset =
            case duration of
                FiveTStates ->
                    5

                NineTStates ->
                    9

                FourteenTStates ->
                    14

                SeventeenTStates ->
                    17

                EighteenTStates ->
                    18

                TwentyTStates ->
                    20

                TwentyThreeTStates ->
                    23

                NineteenTStates ->
                    19

                NumberedTStates int ->
                    int
    in
    { time | cpu_time = time.cpu_time + offset }
