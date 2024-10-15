module RegisterChange exposing (..)

import Z80Types exposing (MainWithIndexRegisters, Z80)


type RegisterChange
    = ChangeRegisterC Int
    | ChangeRegisterBC Int Int
    | ChangeRegisterB Int
    | ChangeRegisterDE Int Int
    | ChangeRegisterE Int
    | ChangeRegisterHL Int
    | ChangeRegisterD Int


applyRegisterChange : RegisterChange -> MainWithIndexRegisters -> MainWithIndexRegisters
applyRegisterChange change main =
    case change of
        ChangeRegisterC int ->
            { main | c = int }

        ChangeRegisterBC b_value c_value ->
            { main | b = b_value, c = c_value }

        ChangeRegisterDE d_value e_value ->
            { main | d = d_value, e = e_value }

        ChangeRegisterE int ->
            { main | e = int }

        ChangeRegisterHL int ->
            { main | hl = int }

        ChangeRegisterB int ->
            { main | b = int }

        ChangeRegisterD int ->
            { main | d = int }
