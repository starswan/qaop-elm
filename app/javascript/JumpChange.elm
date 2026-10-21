module JumpChange exposing (..)

import InstructionDuration exposing (ShortDelay)
import Z80Flags exposing (FlagRegisters)


type JumpChange
    = ConditionalJumpOffset Int ShortDelay (FlagRegisters -> Bool)
    | DJNZOffset Int ShortDelay
