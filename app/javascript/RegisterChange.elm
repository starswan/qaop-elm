module RegisterChange exposing (..)

import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (MainWithIndexRegisters, Z80)


type RegisterChange
    = ChangeRegisterC Int
    | ChangeRegisterBC Int Int
    | ChangeRegisterB Int
    | ChangeRegisterDE Int Int
    | ChangeRegisterE Int
    | ChangeRegisterHL Int
    | ChangeRegisterD Int
    | ChangeRegisterA Int


type RegisterChangeApplied
    = MainRegsApplied MainWithIndexRegisters
    | FlagRegsApplied FlagRegisters


applyRegisterChange : RegisterChange -> FlagRegisters -> MainWithIndexRegisters -> RegisterChangeApplied
applyRegisterChange change z80_flags main =
    case change of
        ChangeRegisterC int ->
            MainRegsApplied { main | c = int }

        ChangeRegisterBC b_value c_value ->
            MainRegsApplied { main | b = b_value, c = c_value }

        ChangeRegisterDE d_value e_value ->
            MainRegsApplied { main | d = d_value, e = e_value }

        ChangeRegisterE int ->
            MainRegsApplied { main | e = int }

        ChangeRegisterHL int ->
            MainRegsApplied { main | hl = int }

        ChangeRegisterB int ->
            MainRegsApplied { main | b = int }

        ChangeRegisterD int ->
            MainRegsApplied { main | d = int }

        ChangeRegisterA int ->
            FlagRegsApplied { z80_flags | a = int }
