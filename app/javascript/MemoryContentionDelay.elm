module MemoryContentionDelay exposing (..)


type CTime
    = NoCont
    | ContUntil Int


type alias CpuTimeCTime =
    { cpu_time : Int
    , ctime : CTime
    }


type MemoryContentionDelay
    = ContentionOne
    | ContentionTwo
    | ContentionThree
    | ContentionFour
    | ContentionFive


addContention : Maybe MemoryContentionDelay -> CpuTimeCTime -> CpuTimeCTime
addContention contention clockTime =
    case contention of
        Just a ->
            let
                inc =
                    case a of
                        ContentionOne ->
                            1

                        ContentionTwo ->
                            2

                        ContentionThree ->
                            3

                        ContentionFour ->
                            4

                        ContentionFive ->
                            5
            in
            clockTime |> addCpuTimeTime inc

        Nothing ->
            clockTime


addCpuTimeTime : Int -> CpuTimeCTime -> CpuTimeCTime
addCpuTimeTime value z80env =
    { z80env | cpu_time = z80env.cpu_time + value }
