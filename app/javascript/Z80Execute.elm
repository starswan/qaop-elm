module Z80Execute exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime)
import RegisterChange exposing (RegisterChange, applyRegisterChange)
import SingleWith8BitParameter exposing (DoubleWithRegisterChange(..), JumpChange, Single8BitChange, applySimple8BitChange)
import Utils exposing (shiftLeftBy8)
import Z80Change exposing (FlagChange(..), applyZ80Change)
import Z80ChangeData exposing (RegisterChangeData, Z80ChangeData)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..), applyDeltaWithChanges)
import Z80Types exposing (IXIYHL(..), Z80)


type DeltaWithChanges
    = OldDeltaWithChanges DeltaWithChangesData
    | PureDelta CpuTimeCTime Z80ChangeData
    | FlagDelta CpuTimeCTime FlagChange
    | RegisterChangeDelta CpuTimeCTime RegisterChangeData
    | Simple8BitDelta CpuTimeCTime Single8BitChange
    | DoubleWithRegistersDelta CpuTimeCTime DoubleWithRegisterChange
    | JumpChangeDelta CpuTimeCTime JumpChange


apply_delta : Z80 -> DeltaWithChanges -> Z80
apply_delta z80 z80delta =
    case z80delta of
        OldDeltaWithChanges deltaWithChangesData ->
            z80 |> applyDeltaWithChanges deltaWithChangesData

        PureDelta cpu_time z80ChangeData ->
            z80 |> applyPureDelta cpu_time z80ChangeData

        FlagDelta cpuTimeCTime flagRegisters ->
            z80 |> applyFlagDelta cpuTimeCTime flagRegisters

        RegisterChangeDelta cpuTimeCTime registerChange ->
            z80 |> applyRegisterDelta cpuTimeCTime registerChange

        Simple8BitDelta cpuTimeCTime single8BitChange ->
            z80 |> applySimple8BitDelta cpuTimeCTime single8BitChange

        DoubleWithRegistersDelta cpuTimeCTime doubleWithRegisterChange ->
            z80 |> applyDoubleWithRegistersDelta cpuTimeCTime doubleWithRegisterChange

        JumpChangeDelta cpuTimeCTime jumpChange ->
            z80 |> applyJumpChangeDelta cpuTimeCTime jumpChange


applyJumpChangeDelta : CpuTimeCTime -> JumpChange -> Z80 -> Z80
applyJumpChangeDelta cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        old_env =
            tmp_z80.env

        pc =
            case z80changeData.jump of
                Just jump ->
                    Bitwise.and (tmp_z80.pc + 2 + jump) 0xFFFF

                Nothing ->
                    Bitwise.and (tmp_z80.pc + 2) 0xFFFF
    in
    { tmp_z80
        | pc = pc
        , env = { old_env | time = cpu_time |> addCpuTimeTime 8 }
        , interrupts = { interrupts | r = interrupts.r + 1 }
    }


applyDoubleWithRegistersDelta : CpuTimeCTime -> DoubleWithRegisterChange -> Z80 -> Z80
applyDoubleWithRegistersDelta cpu_time z80changeData tmp_z80 =
    -- This wouold appear to just be for DJNZ d
    let
        interrupts =
            tmp_z80.interrupts

        old_env =
            tmp_z80.env
    in
    case z80changeData of
        RelativeJumpWithTimeOffset single8BitChange maybeInt timeOffset ->
            let
                pc =
                    case maybeInt of
                        Just jump ->
                            Bitwise.and (tmp_z80.pc + 2 + jump) 0xFFFF

                        Nothing ->
                            Bitwise.and (tmp_z80.pc + 2) 0xFFFF

                main =
                    tmp_z80.main |> applySimple8BitChange single8BitChange
            in
            { tmp_z80
                | main = main
                , pc = pc
                , env = { old_env | time = cpu_time |> addCpuTimeTime timeOffset }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }


applySimple8BitDelta : CpuTimeCTime -> Single8BitChange -> Z80 -> Z80
applySimple8BitDelta cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        z80 =
            { tmp_z80 | env = { env | time = cpu_time }, interrupts = { interrupts | r = interrupts.r + 1 } }

        new_pc =
            Bitwise.and (z80.pc + 2) 0xFFFF

        main =
            z80.main |> applySimple8BitChange z80changeData
    in
    { z80 | pc = new_pc, main = main }


applyFlagDelta : CpuTimeCTime -> FlagChange -> Z80 -> Z80
applyFlagDelta cpu_time z80_flags tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        new_pc =
            Bitwise.and (tmp_z80.pc + 1) 0xFFFF

        z80 =
            { tmp_z80 | pc = new_pc, env = { env | time = cpu_time |> addCpuTimeTime 4 }, interrupts = { interrupts | r = interrupts.r + 1 } }
    in
    case z80_flags of
        OnlyFlags flagRegisters ->
            { z80 | flags = flagRegisters }

        FlagChangeB int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | b = int } }

        FlagChangeC int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | c = int } }

        FlagChangeD int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | d = int } }

        FlagChangeE int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | e = int } }

        FlagChangeH int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | hl = Bitwise.or (shiftLeftBy8 int) (Bitwise.and main.hl 0xFF) } }

        FlagChangeL int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | hl = Bitwise.or int (Bitwise.and main.hl 0xFF00) } }


applyPureDelta : CpuTimeCTime -> Z80ChangeData -> Z80 -> Z80
applyPureDelta cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        z80 =
            { tmp_z80 | env = { env | time = cpu_time |> addCpuTimeTime (4 + z80changeData.cpu_time) }, interrupts = { interrupts | r = interrupts.r + 1 } }

        new_pc =
            Bitwise.and (z80.pc + 1) 0xFFFF
    in
    { z80 | pc = new_pc } |> applyZ80Change z80changeData.changes


applyRegisterDelta : CpuTimeCTime -> RegisterChangeData -> Z80 -> Z80
applyRegisterDelta cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        z80 =
            { tmp_z80 | env = { env | time = cpu_time |> addCpuTimeTime (4 + z80changeData.cpu_time) }, interrupts = { interrupts | r = interrupts.r + 1 } }

        new_pc =
            Bitwise.and (z80.pc + 1) 0xFFFF

        (new_flags, new_main) =
            z80.main |> applyRegisterChange z80changeData.changes z80.flags
    in
    { z80 | pc = new_pc, main = new_main, flags = new_flags }
