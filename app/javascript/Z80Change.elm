module Z80Change exposing (..)

import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Registers exposing (ChangeMainRegister, CoreRegister)


type Z80Change
    = FlagsWithRegisterChange CoreRegister IntWithFlags
    | FlagsWithHLRegister FlagRegisters Int
    | Z80ChangeFlags FlagRegisters
    | Z80ChangeSetIndirect Int Int


type IndexedZ80Change
    = FlagsWithIXRegister FlagRegisters Int
    | FlagsWithIYRegister FlagRegisters Int
    | JustIXRegister Int
    | JustIYRegister Int
