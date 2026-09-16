module CBRegisterChange exposing (..)

import RegisterChange exposing (Shifter)
import Utils exposing (BitTest)
import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Registers exposing (ChangeMainRegister)
import Z80Types exposing (MainWithIndexRegisters)


type CBRegisterFlagChange
    = RegisterChangeShifter Shifter (MainWithIndexRegisters -> Int)
    | TransformMainRegisters (MainWithIndexRegisters -> MainWithIndexRegisters)
    | SetBitIndirectA BitTest Int
    | SetBitIndirectWithCopy BitTest ChangeMainRegister Int
    | FlagsIndirectWithShifter Shifter Int
    | IndirectBitReset BitTest (MainWithIndexRegisters -> Int)
    | ResetBitIndirectWithCopy BitTest ChangeMainRegister Int
    | ResetBitIndirectA BitTest Int
    | IndirectBitSet BitTest (MainWithIndexRegisters -> Int)
    | RegisterIndirectWithShifter (Int -> FlagRegisters -> IntWithFlags) ChangeMainRegister Int
    | RegisterChangeIndexShifter Shifter Int
