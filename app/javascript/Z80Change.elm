module Z80Change exposing (..)

import SingleEnvWithMain exposing (EightBitMain)
import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Registers exposing (ChangeMainRegister, CoreRegister)


type Z80Change
    = FlagsWithRegisterChange CoreRegister IntWithFlags
    | FlagsWithHLRegister FlagRegisters Int
    | FlagsWithIXRegister FlagRegisters Int
    | FlagsWithIYRegister FlagRegisters Int
    | Z80ChangeFlags FlagRegisters
    | Z80ChangeSetIndirect Int Int
    | JustIXRegister Int
    | JustIYRegister Int


type FlagChange
    = OnlyFlags FlagRegisters
    | FlagChange8Bit EightBitMain Int
    | FlagChangeH Int
    | FlagChangeL Int
    | ReturnWithPop
    | EmptyFlagChange
    | FlagChangePush Int
    | FlagNewRValue Int
