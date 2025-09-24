module Z80OpCode exposing (..)

import CpuTimeCTime exposing (CpuTimeAndValue)


type Z80OpCode
    = TimeAndValue CpuTimeAndValue
    | DirectExecute
