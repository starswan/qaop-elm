module RegisterChange exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement)
import Utils exposing (shiftLeftBy8)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (MainWithIndexRegisters, Z80)


type RegisterChange
    = ChangeRegisterCWithTime Int CpuTimeIncrement
    | ChangeRegisterBC Int Int CpuTimeIncrement
    | ChangeRegisterB Int
    | ChangeRegisterDE Int Int CpuTimeIncrement
    | ChangeRegisterEWithTime Int CpuTimeIncrement
    | ChangeRegisterE Int
    | ChangeRegisterHL Int CpuTimeIncrement
    | ChangeRegisterD Int
    | ChangeRegisterA Int
    | ChangeRegisterC Int
    | ChangeRegisterH Int
    | ChangeRegisterL Int


type RegisterChangeApplied
    = MainRegsApplied MainWithIndexRegisters
    | MainRegsWithTimeApplied MainWithIndexRegisters CpuTimeIncrement
    | FlagRegsApplied FlagRegisters


applyRegisterChange : RegisterChange -> FlagRegisters -> MainWithIndexRegisters -> RegisterChangeApplied
applyRegisterChange change z80_flags main =
    case change of
        ChangeRegisterCWithTime int time ->
            MainRegsWithTimeApplied { main | c = int } time

        ChangeRegisterBC b_value c_value time ->
            MainRegsWithTimeApplied { main | b = b_value, c = c_value } time

        ChangeRegisterDE d_value e_value time ->
            MainRegsWithTimeApplied { main | d = d_value, e = e_value } time

        ChangeRegisterEWithTime int time ->
            MainRegsWithTimeApplied { main | e = int } time

        ChangeRegisterHL int time ->
            MainRegsWithTimeApplied { main | hl = int } time

        ChangeRegisterB int ->
            MainRegsApplied { main | b = int }

        ChangeRegisterD int ->
            MainRegsApplied { main | d = int }

        ChangeRegisterA int ->
            FlagRegsApplied { z80_flags | a = int }

        ChangeRegisterE int ->
            MainRegsApplied { main | e = int }


        ChangeRegisterC int ->
            MainRegsApplied { main | c = int }

        ChangeRegisterH int ->
            MainRegsApplied { main | hl = Bitwise.or (Bitwise.and main.hl 0xFF) (shiftLeftBy8 int) }


        ChangeRegisterL int ->
            MainRegsApplied { main | hl = Bitwise.or (Bitwise.and main.hl 0xFF00) int }


