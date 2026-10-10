module Z80Change exposing (..)

import Utils exposing (BitTest)
import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Registers exposing (ChangeMainRegister, CoreRegister)
import Z80Types exposing (MainWithIndexRegisters)


type Shifter
    = Shifter0
    | Shifter1
    | Shifter2
    | Shifter3
    | Shifter4
    | Shifter5
    | Shifter6
    | Shifter7


type Z80Change
    = FlagsWithRegisterChange CoreRegister ( Int, FlagRegisters )
    | FlagsWithHLRegister FlagRegisters Int
    | Z80ChangeFlags FlagRegisters
    | Z80ChangeSetIndirect Int Int
    | Z80FlagChangeFunc (FlagRegisters -> FlagRegisters)
    | RegisterChangeShifter Shifter (MainWithIndexRegisters -> Int)
    | Z80IndirectMainBitTest BitTest (MainWithIndexRegisters -> Int)
    | FlagRegChangeFunc (MainWithIndexRegisters -> FlagRegisters -> ( Int, FlagRegisters )) ChangeMainRegister


type IndexedZ80Change
    = FlagsWithIXRegister FlagRegisters Int
    | FlagsWithIYRegister FlagRegisters Int
    | JustIXRegister Int
    | JustIYRegister Int
