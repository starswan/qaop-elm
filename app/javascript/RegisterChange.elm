module RegisterChange exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement)
import Utils exposing (shiftLeftBy8)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (MainWithIndexRegisters, Z80, set_de_main)


type Shifter
    = Shifter0
    | Shifter1
    | Shifter2
    | Shifter3
    | Shifter4
    | Shifter5
    | Shifter6
    | Shifter7


type RegisterChange
    = ChangeRegisterCWithTime Int CpuTimeIncrement
    | ChangeRegisterBC Int Int CpuTimeIncrement
    | ChangeRegisterB Int
    | ChangeRegisterDE Int Int CpuTimeIncrement
    | ChangeRegisterE Int
    | ChangeRegisterHL Int CpuTimeIncrement
    | ChangeRegisterIX Int CpuTimeIncrement
    | ChangeRegisterIY Int CpuTimeIncrement
    | ChangeRegisterD Int
    | ChangeRegisterA Int
    | ChangeRegisterC Int
    | ChangeRegisterH Int
    | ChangeRegisterL Int
    | PushedValue Int
    | RegChangeNewSP Int CpuTimeIncrement
    | IncrementIndirect Int CpuTimeIncrement
    | DecrementIndirect Int CpuTimeIncrement
    | RegisterChangeJump Int
    | SetIndirect Int Int CpuTimeIncrement
    | ChangeRegisterDEAndHL Int Int
    | RegisterChangeShifter Shifter Int CpuTimeIncrement


type RegisterChangeApplied
    = MainRegsApplied MainWithIndexRegisters
    | MainRegsWithTimeApplied MainWithIndexRegisters CpuTimeIncrement
    | FlagRegsApplied FlagRegisters
    | PushedValueApplied Int
    | NewSPApplied Int CpuTimeIncrement
    | IncrementIndirectApplied Int CpuTimeIncrement
    | DecrementIndirectApplied Int CpuTimeIncrement
    | JumpApplied Int
    | SetIndirectApplied Int Int CpuTimeIncrement
    | RegisterChangeShifterApplied Shifter Int CpuTimeIncrement


applyRegisterChange : RegisterChange -> FlagRegisters -> MainWithIndexRegisters -> RegisterChangeApplied
applyRegisterChange change z80_flags main =
    case change of
        ChangeRegisterCWithTime int time ->
            MainRegsWithTimeApplied { main | c = int } time

        ChangeRegisterBC b_value c_value time ->
            MainRegsWithTimeApplied { main | b = b_value, c = c_value } time

        ChangeRegisterDE d_value e_value time ->
            MainRegsWithTimeApplied { main | d = d_value, e = e_value } time

        ChangeRegisterHL int time ->
            MainRegsWithTimeApplied { main | hl = int } time

        ChangeRegisterIX int cpuTimeIncrement ->
            MainRegsWithTimeApplied { main | ix = int } cpuTimeIncrement

        ChangeRegisterIY int cpuTimeIncrement ->
            MainRegsWithTimeApplied { main | iy = int } cpuTimeIncrement

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

        PushedValue int ->
            PushedValueApplied int

        RegChangeNewSP int cpuTimeIncrement ->
            NewSPApplied int cpuTimeIncrement

        IncrementIndirect int cpuTimeIncrement ->
            IncrementIndirectApplied int cpuTimeIncrement

        DecrementIndirect int cpuTimeIncrement ->
            DecrementIndirectApplied int cpuTimeIncrement

        RegisterChangeJump int ->
            JumpApplied int

        SetIndirect addr value cpuTimeIncrement ->
            SetIndirectApplied addr value cpuTimeIncrement

        ChangeRegisterDEAndHL de hl ->
            MainRegsApplied ({ main | hl = hl } |> set_de_main de)

        RegisterChangeShifter shifter int cpuTimeIncrement ->
            RegisterChangeShifterApplied shifter int cpuTimeIncrement
