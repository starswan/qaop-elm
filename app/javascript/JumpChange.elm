module JumpChange exposing (..)

import CpuTimeCTime exposing (ShortDelay)
import Z80Core exposing (CoreChange(..), Z80Core)
import Z80Flags exposing (FlagRegisters)


type TripleWithFlagsChange
    = Conditional16BitJump Int (FlagRegisters -> Bool)
    | Conditional16BitCall Int ShortDelay (FlagRegisters -> Bool)
    | NewPCRegister Int


applyTripleFlagChange : TripleWithFlagsChange -> Z80Core -> CoreChange
applyTripleFlagChange z80changeData z80 =
    case z80changeData of
        Conditional16BitJump int function ->
            if z80.flags |> function then
                JumpOnlyPC int

            else
                NoCore

        Conditional16BitCall address shortdelay function ->
            if z80.flags |> function then
                CallWithPCAndDelay address shortdelay

            else
                NoCore

        NewPCRegister int ->
            JumpOnlyPC int
