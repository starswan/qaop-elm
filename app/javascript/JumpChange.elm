module JumpChange exposing (..)

import TripleWithFlags exposing (TripleWithFlagsChange(..))
import Z80Core exposing (CoreChange(..), Z80Core)


applyTripleFlagChange : TripleWithFlagsChange -> Z80Core -> CoreChange
applyTripleFlagChange z80changeData z80 =
    case z80changeData of
        Conditional16BitJump int function ->
            if z80.flags |> function then
                JumpOnlyPC int

            else
                z80 |> CoreOnly

        Conditional16BitCall address shortdelay function ->
            if z80.flags |> function then
                CallWithPCAndDelay address shortdelay

            else
                z80 |> CoreOnly

        CallImmediate int ->
            CallWithPC int

        NewPCRegister int ->
            JumpOnlyPC int
