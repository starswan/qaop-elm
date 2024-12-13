module RegisterChange exposing (..)

import TransformTypes exposing (InstructionDuration(..))
import Z80Flags exposing (FlagFunc, FlagRegisters, changeFlags)
import Z80Transform exposing (ChangeEnvOperation(..), ChangeEnvWithFlagsOperation(..), ChangeMainOperation(..), InstructionLength, Z80Operation(..))
import Z80Types exposing (MainWithIndexRegisters, Z80)


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
    | SingleEnvFlagFunc FlagFunc Int
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
    | RegisterChangeShifter Shifter Int


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

        SingleEnvFlagFunc flagFunc value ->
            FlagRegsApplied (z80_flags |> changeFlags flagFunc value)

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
            OperationApplied (ChangeMain (ChangeIYLRegister int))

        ChangeRegisterIYH int ->
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
            OperationApplied (ChangeMain (ChangeMainDEAndHL de hl))

        RegisterChangeShifter shifter addr ->
            if addr >= 0x4000 then
                let
                    ram_addr =
                        addr - 0x4000

                    apply =
                        case shifter of
                            Shifter0 ->
                                ApplyShifter0 ram_addr

                            Shifter1 ->
                                ApplyShifter1 ram_addr

                            Shifter2 ->
                                ApplyShifter2 ram_addr

                            Shifter3 ->
                                ApplyShifter3 ram_addr

                            Shifter4 ->
                                ApplyShifter4 ram_addr

                            Shifter5 ->
                                ApplyShifter5 ram_addr

                            Shifter6 ->
                                ApplyShifter6 ram_addr

                            Shifter7 ->
                                ApplyShifter7 ram_addr
                in
                OperationWithDurationApplied (ChangeEnvWithFlags apply) SevenTStates

            else
                OperationApplied ChangeNothing
