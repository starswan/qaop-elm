module JumpChange exposing (..)

import CpuTimeCTime exposing (ShortDelay)
import Z80Core exposing (CoreChange(..), Z80Core)
import Z80Flags exposing (FlagRegisters)


type JumpChange
    = ConditionalJumpOffset Int ShortDelay (FlagRegisters -> Bool)
    | DJNZOffset Int ShortDelay
