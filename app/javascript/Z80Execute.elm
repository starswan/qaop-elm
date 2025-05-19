module Z80Execute exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement(..))
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import SingleNoParams exposing (NoParamChange(..))
import TransformTypes exposing (InstructionDuration(..))
import Utils exposing (shiftLeftBy8)
import Z80Change exposing (FlagChange(..), Z80Change)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..))
import Z80Env exposing (Z80Env, z80_pop)
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeEnvOperation(..), ChangeMainOperation(..), ChangeMemoryOperation(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (IXIYHL(..), MainWithIndexRegisters, Z80)



--type DeltaWithChanges
--    = OldDeltaWithChanges DeltaWithChangesData
--    | NoParamsDelta CpuTimeCTime NoParamChange


type ExecuteResult
    = Z80DeltaChange DeltaWithChangesData
    | NoParamsDeltaChange CpuTimeCTime NoParamChange
    | Transformer Z80Transform


applyFlagDelta : PCIncrement -> CpuTimeCTime -> FlagChange -> Z80ROM -> Z80 -> Z80Transform
applyFlagDelta pcInc instrTime flagChange rom48k z80 =
    let
        --interrupts =
        --    tmp_z80.interrupts
        --
        --env =
        --    tmp_z80.env
        new_pc =
            case pcInc of
                IncrementByOne ->
                    --(tmp_z80.pc + 1) |> Bitwise.and 0xFFFF
                    OneByteInstruction

                IncrementByTwo ->
                    --(tmp_z80.pc + 2) |> Bitwise.and 0xFFFF
                    TwoByteInstruction

        --z80 =
        --    { tmp_z80 | pc = new_pc, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }
    in
    case flagChange of
        OnlyFlags flagRegisters ->
            --{ z80 | flags = flagRegisters }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeFlagRegisters flagRegisters }

        FlagChangeB int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | b = int } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeBRegister int) }

        FlagChangeC int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | c = int } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeCRegister int) }

        FlagChangeD int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | d = int } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeDRegister int) }

        FlagChangeE int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | e = int } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeERegister int) }

        FlagChangeH int ->
            let
                main =
                    z80.main

                hl =
                    Bitwise.or (shiftLeftBy8 int) (Bitwise.and main.hl 0xFF)
            in
            --{ z80 | main = { main | hl = Bitwise.or (shiftLeftBy8 int) (Bitwise.and main.hl 0xFF) } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeHLRegister hl) }

        FlagChangeL int ->
            let
                main =
                    z80.main

                hl =
                    Bitwise.or int (Bitwise.and main.hl 0xFF00)
            in
            --{ z80 | main = { main | hl = Bitwise.or int (Bitwise.and main.hl 0xFF00) } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeHLRegister hl) }

        ReturnWithPop ->
            let
                result =
                    z80.env |> z80_pop rom48k

                --env1 =
                --    z80.env
                --x = debug_log "ret nz" (result.value |> subName) Nothing
            in
            --{ z80 | pc = result.value16, env = { env1 | time = result.time |> addCpuTimeTimeInc timeIncrement, sp = result.sp } }
            { pcIncrement = JumpInstruction result.value16, time = instrTime, timeIncrement = FiveTStates, operation = ChangeEnv (ChangeSPRegister result.sp) }

        EmptyFlagChange ->
            --let
            --    env1 =
            --        z80.env |> addCpuTimeEnvInc timeIncrement
            --in
            --{ z80 | env = env1 }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SevenTStates, operation = ChangeNothing }

        FlagChangePush int ->
            --{ z80 | env = env |> z80_push int }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeEnv (PushValue int) }
