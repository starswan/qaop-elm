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


applyRegisterChange : RegisterChange -> FlagRegisters -> MainWithIndexRegisters -> (FlagRegisters, MainWithIndexRegisters)
applyRegisterChange change z80_flags main =
    case change of
        ChangeRegisterC int ->
            (z80_flags, { main | c = int })

        ChangeRegisterBC b_value c_value ->
            (z80_flags, { main | b = b_value, c = c_value })

        ChangeRegisterDE d_value e_value ->
            (z80_flags,{ main | d = d_value, e = e_value })

        ChangeRegisterE int ->
            (z80_flags, { main | e = int })

        ChangeRegisterHL int ->
            (z80_flags, { main | hl = int })

        ChangeRegisterB int ->
            (z80_flags, { main | b = int })

        ChangeRegisterD int ->
            (z80_flags, { main | d = int })

        ChangeRegisterA int ->
            ({z80_flags | a = int}, main)
