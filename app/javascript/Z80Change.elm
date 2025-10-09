module Z80Change exposing (..)

import RegisterChange exposing (ChangeMainRegister)
import SingleEnvWithMain exposing (EightBitMain)
import Z80Flags exposing (FlagRegisters, IntWithFlags)


type Z80Change
    = FlagsWithRegisterChange ChangeMainRegister IntWithFlags
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
