module RegisterChange exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement)
import TransformTypes exposing (InstructionDuration(..))
import Utils exposing (shiftLeftBy8)
import Z80Flags exposing (FlagRegisters, adc, sbc, z80_add, z80_sub)
import Z80Transform exposing (ChangeEnvOperation(..), ChangeEnvWithFlagsOperation(..), ChangeMainOperation(..), InstructionLength, Z80Operation(..))
import Z80Types exposing (MainWithIndexRegisters, Z80, set_de_main)


type RegisterChange
    = ChangeRegisterA Int
    | ChangeRegisterBC Int Int
    | ChangeRegisterB Int
    | ChangeRegisterC Int
    | ChangeRegisterC6TStates Int
    | ChangeRegisterDE Int Int
    | ChangeRegisterE6States Int
    | ChangeRegisterE Int
    | ChangeRegisterHL Int
    | ChangeRegisterIX Int
    | ChangeRegisterIY Int
    | ChangeRegisterD Int
    | AddToRegisterA Int
    | AdcRegisterA Int
    | SubRegisterA Int
    | SbcRegisterA Int
    | ChangeRegisterH Int
    | ChangeRegisterIXH Int
    | ChangeRegisterIXL Int
    | ChangeRegisterIYH Int
    | ChangeRegisterIYL Int
    | ChangeRegisterL Int
    | PushedValue Int
    | RegChangeNewSP Int
    | IncrementIndirect Int
    | DecrementIndirect Int
    | RegisterChangeJump Int
    | SetIndirect Int Int
    | ChangeRegisterDEAndHL Int Int
    | Shifter0 Int
    | Shifter1 Int
    | Shifter2 Int
    | Shifter3 Int
    | Shifter4 Int
    | Shifter5 Int
    | Shifter6 Int
    | Shifter7 Int


type RegisterChangeApplied
    = FlagRegsApplied FlagRegisters
    | IncrementIndirectApplied Int
    | DecrementIndirectApplied Int
    | JumpApplied Int
    | OperationApplied Z80Operation
    | OperationWithDurationApplied Z80Operation InstructionDuration


applyRegisterChange : RegisterChange -> FlagRegisters -> MainWithIndexRegisters -> RegisterChangeApplied
applyRegisterChange change z80_flags z80_main =
    case change of
        ChangeRegisterC6TStates int ->
            OperationWithDurationApplied (ChangeMain (ChangeCRegister int)) SixTStates

        ChangeRegisterBC b_value c_value ->
            OperationWithDurationApplied (ChangeMain (ChangeRegisterBAndC b_value c_value)) SixTStates

        ChangeRegisterDE d_value e_value ->
            OperationWithDurationApplied (ChangeMain (ChangeRegisterDAndE d_value e_value)) SixTStates

        ChangeRegisterE6States int ->
            OperationWithDurationApplied (ChangeMain (ChangeERegister int)) SixTStates

        ChangeRegisterHL int ->
            OperationWithDurationApplied (ChangeMain (ChangeHLRegister int)) SixTStates

        ChangeRegisterIX int ->
            OperationWithDurationApplied (ChangeMain (ChangeIXRegister int)) SixTStates

        ChangeRegisterIY int ->
            OperationWithDurationApplied (ChangeMain (ChangeIYRegister int)) SixTStates

        ChangeRegisterB int ->
            OperationApplied (ChangeMain (ChangeBRegister int))

        ChangeRegisterD int ->
            OperationApplied (ChangeMain (ChangeDRegister int))

        ChangeRegisterA int ->
            FlagRegsApplied { z80_flags | a = int }

        AddToRegisterA value ->
            FlagRegsApplied (z80_flags |> z80_add value)

        AdcRegisterA value ->
            FlagRegsApplied (z80_flags |> adc value)

        SubRegisterA value ->
            FlagRegsApplied (z80_flags |> z80_sub value)

        SbcRegisterA value ->
            FlagRegsApplied (z80_flags |> sbc value)

        ChangeRegisterE int ->
            OperationApplied (ChangeMain (ChangeERegister int))

        ChangeRegisterC int ->
            OperationApplied (ChangeMain (ChangeCRegister int))

        ChangeRegisterH int ->
            OperationApplied (ChangeMain (ChangeHRegister int))

        ChangeRegisterIXH int ->
            OperationApplied (ChangeMain (ChangeIXHRegister int))

        ChangeRegisterIXL int ->
            OperationApplied (ChangeMain (ChangeIXLRegister int))

        ChangeRegisterIYL int ->
            --MainRegsApplied { z80_main | iy = Bitwise.or (Bitwise.and z80_main.iy 0xFF00) int }
            OperationApplied (ChangeMain (ChangeIYLRegister int))
        ChangeRegisterIYH int ->
            --MainRegsApplied { z80_main | iy = Bitwise.or (Bitwise.and z80_main.iy 0xFF) (shiftLeftBy8 int) }
            OperationApplied (ChangeMain (ChangeIYHRegister int))

        ChangeRegisterL int ->
            OperationApplied (ChangeMain (ChangeLRegister int))

        PushedValue int ->
            OperationApplied (ChangeEnv (PushValue int))

        RegChangeNewSP int ->
            OperationApplied (ChangeEnv (ChangeSPRegister int))

        IncrementIndirect int ->
            IncrementIndirectApplied int

        DecrementIndirect int ->
            DecrementIndirectApplied int

        RegisterChangeJump int ->
            JumpApplied int

        SetIndirect addr value ->
            OperationWithDurationApplied (ChangeEnv (Store8BitMemoryValue addr value)) SevenTStates

        ChangeRegisterDEAndHL de hl ->
            --MainRegsApplied ({ z80_main | hl = hl } |> set_de_main de)
            OperationApplied (ChangeMain (ChangeMainDEAndHL de hl))

        Shifter0 addr ->
            if addr >= 0x4000 then
                OperationWithDurationApplied (ChangeEnvWithFlags (ApplyShifter0 (addr - 0x4000))) SevenTStates

            else
                OperationApplied ChangeNothing

        Shifter1 addr ->
            if addr >= 0x4000 then
                OperationWithDurationApplied (ChangeEnvWithFlags (ApplyShifter1 (addr - 0x4000))) SevenTStates

            else
                OperationApplied ChangeNothing

        Shifter2 addr ->
            if addr >= 0x4000 then
                OperationWithDurationApplied (ChangeEnvWithFlags (ApplyShifter2 (addr - 0x4000))) SevenTStates

            else
                OperationApplied ChangeNothing

        Shifter3 addr ->
            if addr >= 0x4000 then
                OperationWithDurationApplied (ChangeEnvWithFlags (ApplyShifter3 (addr - 0x4000))) SevenTStates

            else
                OperationApplied ChangeNothing

        Shifter4 addr ->
            if addr >= 0x4000 then
                OperationWithDurationApplied (ChangeEnvWithFlags (ApplyShifter4 (addr - 0x4000))) SevenTStates

            else
                OperationApplied ChangeNothing

        Shifter5 addr ->
            if addr >= 0x4000 then
                OperationWithDurationApplied (ChangeEnvWithFlags (ApplyShifter5 (addr - 0x4000))) SevenTStates

            else
                OperationApplied ChangeNothing

        Shifter6 addr ->
            if addr >= 0x4000 then
                OperationWithDurationApplied (ChangeEnvWithFlags (ApplyShifter6 (addr - 0x4000))) SevenTStates

            else
                OperationApplied ChangeNothing

        Shifter7 addr ->
            if addr >= 0x4000 then
                OperationWithDurationApplied (ChangeEnvWithFlags (ApplyShifter7 (addr - 0x4000))) SevenTStates

            else
                OperationApplied ChangeNothing
