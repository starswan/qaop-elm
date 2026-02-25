module Z80Change exposing (..)

import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Registers exposing (ChangeMainRegister, CoreRegister)
import Z80Types exposing (MainWithIndexRegisters)


type Z80Change
    = FlagsWithRegisterChange CoreRegister IntWithFlags
    | FlagsWithHLRegister FlagRegisters Int
    | FlagsWithIXRegister FlagRegisters Int
    | FlagsWithIYRegister FlagRegisters Int
    | Z80ChangeFlags FlagRegisters
    | Z80ChangeSetIndirect Int Int
    | JustIXRegister Int
    | JustIYRegister Int
