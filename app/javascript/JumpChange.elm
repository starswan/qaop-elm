module JumpChange exposing (..)

import CpuTimeCTime exposing (ShortDelay)
import Z80Core exposing (CoreChange(..), Z80Core)
import Z80Flags exposing (FlagRegisters)


type TripleWithFlagsChange
    = Conditional16BitJump Int (FlagRegisters -> Bool)
    | Conditional16BitCall Int ShortDelay (FlagRegisters -> Bool)
    | NewPCRegister Int


type JumpChange
    = ConditionalJumpOffset Int ShortDelay (FlagRegisters -> Bool)
    | DJNZOffset Int ShortDelay


applyTripleFlagChange : TripleWithFlagsChange -> FlagRegisters -> CoreChange
applyTripleFlagChange z80changeData z80_flags =
    case z80changeData of
        Conditional16BitJump int function ->
            if z80_flags |> function then
                JumpOnlyPC int

            else
                NoCore

        Conditional16BitCall address shortdelay function ->
            if z80_flags |> function then
                CallWithPCAndDelay address shortdelay

            else
                NoCore

        NewPCRegister int ->
            JumpOnlyPC int
