module InstructionDuration exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)


type InstructionDuration
    = NineteenTStates
    | NumberedTStates Int


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
