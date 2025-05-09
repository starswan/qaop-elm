module Z80Execute exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement(..), addCpuTimeTime, addCpuTimeTimeInc, cpuTimeIncrement4)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import RegisterChange exposing (RegisterChange, RegisterChangeApplied(..), applyRegisterChange)
import SingleByteWithEnv exposing (SingleByteEnvChange(..), applyEnvChangeDelta)
import SingleEnvWithMain exposing (SingleEnvMainChange, applySingleEnvMainChange)
import SingleNoParams exposing (NoParamChange(..), applyNoParamsDelta)
import SingleWith8BitParameter exposing (DoubleWithRegisterChange(..), JumpChange(..), Single8BitChange, applySimple8BitChange)
import TripleByte exposing (TripleByteChange(..))
import TripleWithFlags exposing (TripleWithFlagsChange(..))
import TripleWithMain exposing (TripleMainChange, applyTripleMainChange)
import Utils exposing (shiftLeftBy8)
import Z80Change exposing (FlagChange(..), Z80Change, applyZ80Change)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..), applyDeltaWithChanges)
import Z80Env exposing (Z80Env, addCpuTimeEnvInc, mem, mem16, setMem, z80_pop, z80_push)
import Z80Flags exposing (FlagRegisters, IntWithFlags, dec, inc, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), MainWithIndexRegisters, Z80, set_bc_main, set_de_main)


type DeltaWithChanges
    = OldDeltaWithChanges DeltaWithChangesData
    | PureDelta PCIncrement CpuTimeCTime Z80Change
    | FlagDelta PCIncrement CpuTimeCTime FlagChange
    | RegisterChangeDelta PCIncrement CpuTimeCTime RegisterChange
    | Simple8BitDelta MediumPCIncrement CpuTimeCTime Single8BitChange
    | DoubleWithRegistersDelta MediumPCIncrement CpuTimeCTime DoubleWithRegisterChange
    | JumpChangeDelta CpuTimeCTime JumpChange
    | NoParamsDelta CpuTimeCTime NoParamChange
    | SingleEnvDelta CpuTimeCTime SingleByteEnvChange
    | MainWithEnvDelta PCIncrement SingleEnvMainChange
    | TripleMainChangeDalta CpuTimeCTime TriplePCIncrement TripleMainChange


type ExecuteResult
    = Z80DeltaChange DeltaWithChanges
    | Z80Result Z80


apply_delta : Z80 -> Z80ROM -> DeltaWithChanges -> Z80
apply_delta z80 rom48k z80delta =
    case z80delta of
        OldDeltaWithChanges deltaWithChangesData ->
            z80 |> applyDeltaWithChanges deltaWithChangesData

        PureDelta cpuInc cpu_time z80ChangeData ->
            z80 |> applyPureDelta cpuInc cpu_time z80ChangeData

        FlagDelta pcInc cpuTimeCTime flagRegisters ->
            z80 |> applyFlagDelta pcInc cpuTimeCTime flagRegisters rom48k

        RegisterChangeDelta pcInc cpuTimeCTime registerChange ->
            z80 |> applyRegisterDelta pcInc cpuTimeCTime registerChange rom48k

        Simple8BitDelta pcInc cpuTimeCTime single8BitChange ->
            z80 |> applySimple8BitDelta pcInc cpuTimeCTime single8BitChange

        DoubleWithRegistersDelta pcInc cpuTimeCTime doubleWithRegisterChange ->
            z80 |> applyDoubleWithRegistersDelta pcInc cpuTimeCTime doubleWithRegisterChange

        JumpChangeDelta cpuTimeCTime jumpChange ->
            z80 |> applyJumpChangeDelta cpuTimeCTime jumpChange

        NoParamsDelta cpuTimeCTime noParamChange ->
            z80 |> applyNoParamsDelta cpuTimeCTime noParamChange rom48k

        SingleEnvDelta cpuTimeCTime singleByteEnvChange ->
            z80 |> applyEnvChangeDelta cpuTimeCTime singleByteEnvChange

        MainWithEnvDelta pcInc singleEnvMainChange ->
            z80 |> applySingleEnvMainChange pcInc singleEnvMainChange

        TripleMainChangeDalta cpuTimeCTime triplePCIncrement tripleMainChange ->
            z80 |> applyTripleMainChange cpuTimeCTime triplePCIncrement tripleMainChange


applyJumpChangeDelta : CpuTimeCTime -> JumpChange -> Z80 -> Z80
applyJumpChangeDelta cpu_time z80changeData z80 =
    let
        old_env =
            z80.env
    in
    case z80changeData of
        ActualJump jump ->
            let
                pc =
                    Bitwise.and (z80.pc + 2 + jump) 0xFFFF
            in
            { z80
                | pc = pc
                , env = { old_env | time = cpu_time |> addCpuTimeTime 8 }
                , r = z80.r + 1
            }

        NoJump ->
            let
                pc =
                    Bitwise.and (z80.pc + 2) 0xFFFF
            in
            { z80
                | pc = pc
                , env = { old_env | time = cpu_time |> addCpuTimeTime 8 }
                , r = z80.r + 1
            }

        FlagJump flags ->
            let
                pc =
                    Bitwise.and (z80.pc + 2) 0xFFFF
            in
            { z80
                | pc = pc
                , flags = flags
                , env = { old_env | time = cpu_time }
                , r = z80.r + 1
            }


applyDoubleWithRegistersDelta : MediumPCIncrement -> CpuTimeCTime -> DoubleWithRegisterChange -> Z80 -> Z80
applyDoubleWithRegistersDelta pc_inc cpu_time z80changeData z80 =
    let
        old_env =
            z80.env

        new_pc =
            case pc_inc of
                IncreaseByTwo ->
                    z80.pc + 2

                IncreaseByThree ->
                    z80.pc + 3
    in
    case z80changeData of
        RelativeJumpWithTimeOffset single8BitChange maybeInt timeOffset ->
            let
                pc =
                    case maybeInt of
                        Just jump ->
                            Bitwise.and (new_pc + jump) 0xFFFF

                        Nothing ->
                            Bitwise.and new_pc 0xFFFF

                main =
                    z80.main |> applySimple8BitChange single8BitChange
            in
            { z80
                | main = main
                , pc = pc
                , env = { old_env | time = cpu_time |> addCpuTimeTime timeOffset }
                , r = z80.r + 1
            }

        DoubleRegChangeStoreIndirect addr value cpuTimeIncrement ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time } |> setMem addr value
            in
            { z80
                | pc = pc
                , env = env_1 |> addCpuTimeEnvInc cpuTimeIncrement
                , r = z80.r + 1
            }

        NewHLRegisterValue int ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main
            in
            { z80
                | pc = pc
                , env = env_1
                , main = { main | hl = int }
                , r = z80.r + 1
            }

        NewIXRegisterValue int ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main
            in
            { z80
                | pc = pc
                , env = env_1
                , main = { main | ix = int }
                , r = z80.r + 1
            }

        NewIYRegisterValue int ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main
            in
            { z80
                | pc = pc
                , env = env_1
                , main = { main | iy = int }
                , r = z80.r + 1
            }


applySimple8BitDelta : MediumPCIncrement -> CpuTimeCTime -> Single8BitChange -> Z80 -> Z80
applySimple8BitDelta pcInc cpu_time z80changeData z80 =
    let
        z80_env =
            z80.env

        --z80 =
        --    { tmp_z80 | env = { z80_env | time = cpu_time } }
        new_pc =
            case pcInc of
                IncreaseByTwo ->
                    Bitwise.and (z80.pc + 2) 0xFFFF

                IncreaseByThree ->
                    Bitwise.and (z80.pc + 3) 0xFFFF

        main =
            z80.main |> applySimple8BitChange z80changeData
    in
    { z80 | pc = new_pc, r = z80.r + 1, main = main, env = { z80_env | time = cpu_time } }


applyFlagDelta : PCIncrement -> CpuTimeCTime -> FlagChange -> Z80ROM -> Z80 -> Z80
applyFlagDelta pcInc cpu_time z80_flags rom48k tmp_z80 =
    let
        env =
            tmp_z80.env

        new_pc =
            case pcInc of
                IncrementByOne ->
                    (tmp_z80.pc + 1) |> Bitwise.and 0xFFFF

                IncrementByTwo ->
                    (tmp_z80.pc + 2) |> Bitwise.and 0xFFFF

        z80 =
            { tmp_z80 | pc = new_pc, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, r = tmp_z80.r + 1 }
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

        ReturnWithPop timeIncrement ->
            let
                result =
                    z80.env |> z80_pop rom48k

                env1 =
                    z80.env

                --x = debug_log "ret nz" (result.value |> subName) Nothing
            in
            { z80 | pc = result.value, env = { env1 | time = result.time |> addCpuTimeTimeInc timeIncrement, sp = result.sp } }

        EmptyFlagChange timeIncrement ->
            let
                env1 =
                    z80.env |> addCpuTimeEnvInc timeIncrement
            in
            { z80 | env = env1 }

        FlagChangePush int ->
            { z80 | env = env |> z80_push int }


applyPureDelta : PCIncrement -> CpuTimeCTime -> Z80Change -> Z80 -> Z80
applyPureDelta cpuInc cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        z80 =
            { tmp_z80 | env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 } }

        new_pc =
            case cpuInc of
                IncrementByOne ->
                    (z80.pc + 1) |> Bitwise.and 0xFFFF

                IncrementByTwo ->
                    (z80.pc + 2) |> Bitwise.and 0xFFFF
    in
    { z80 | pc = new_pc, r = z80.r + 1 } |> applyZ80Change z80changeData


applyRegisterDelta : PCIncrement -> CpuTimeCTime -> RegisterChange -> Z80ROM -> Z80 -> Z80
applyRegisterDelta pc_inc cpu_time z80changeData rom48k z80 =
    let
        env =
            z80.env

        new_pc =
            case pc_inc of
                IncrementByOne ->
                    Bitwise.and (z80.pc + 1) 0xFFFF

                IncrementByTwo ->
                    Bitwise.and (z80.pc + 2) 0xFFFF
    in
    case z80.main |> applyRegisterChange z80changeData z80.flags of
        MainRegsApplied new_main ->
            { z80
                | pc = new_pc
                , main = new_main
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        FlagRegsApplied new_flags ->
            { z80
                | pc = new_pc
                , flags = new_flags
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        MainRegsWithTimeApplied mainWithIndexRegisters timeIncrement ->
            let
                env_1 =
                    { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 |> addCpuTimeTimeInc timeIncrement }
            in
            { z80
                | env = env_1
                , pc = new_pc
                , main = mainWithIndexRegisters
                , r = z80.r + 1
            }

        PushedValueApplied int ->
            let
                env1 =
                    env |> z80_push int
            in
            { z80
                | pc = new_pc
                , env = { env1 | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        NewSPApplied int cpuTimeIncrement ->
            { z80
                | pc = new_pc
                , env = { env | sp = int, time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        JumpApplied int ->
            { z80
                | pc = int
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        IncrementIndirectApplied addr cpuTimeIncrement ->
            -- This should be a primitive operation on Z80Env to increment a stored value
            let
                value =
                    mem addr cpu_time rom48k z80.env.ram

                env_2 =
                    { env | time = value.time }

                flags =
                    z80.flags |> inc value.value

                env_3 =
                    env_2 |> setMem addr flags.value |> addCpuTimeEnvInc cpuTimeIncrement
            in
            { z80 | pc = new_pc, env = env_3, flags = flags.flags, r = z80.r + 1 }

        DecrementIndirectApplied addr cpuTimeIncrement ->
            -- This should be a primitive operation on Z80Env to decrement a stored value
            let
                value =
                    mem addr cpu_time rom48k z80.env.ram

                env_2 =
                    { env | time = value.time }

                flags =
                    z80.flags |> dec value.value

                env_3 =
                    env_2 |> setMem addr flags.value |> addCpuTimeEnvInc cpuTimeIncrement
            in
            { z80 | pc = new_pc, env = env_3, flags = flags.flags, r = z80.r + 1 }

        SetIndirectApplied addr value cpuTimeIncrement ->
            let
                env_1 =
                    env |> setMem addr value |> addCpuTimeEnvInc cpuTimeIncrement
            in
            { z80 | pc = new_pc, env = env_1, r = z80.r + 1 }

        Shifter0Applied addr cpuTimeIncrement ->
            z80 |> applyShifter new_pc shifter0 addr cpuTimeIncrement cpu_time rom48k

        Shifter1Applied addr cpuTimeIncrement ->
            z80 |> applyShifter new_pc shifter1 addr cpuTimeIncrement cpu_time rom48k

        Shifter2Applied addr cpuTimeIncrement ->
            z80 |> applyShifter new_pc shifter2 addr cpuTimeIncrement cpu_time rom48k

        Shifter3Applied addr cpuTimeIncrement ->
            z80 |> applyShifter new_pc shifter3 addr cpuTimeIncrement cpu_time rom48k

        Shifter4Applied addr cpuTimeIncrement ->
            z80 |> applyShifter new_pc shifter4 addr cpuTimeIncrement cpu_time rom48k

        Shifter5Applied addr cpuTimeIncrement ->
            z80 |> applyShifter new_pc shifter5 addr cpuTimeIncrement cpu_time rom48k

        Shifter6Applied addr cpuTimeIncrement ->
            z80 |> applyShifter new_pc shifter6 addr cpuTimeIncrement cpu_time rom48k

        Shifter7Applied addr cpuTimeIncrement ->
            z80 |> applyShifter new_pc shifter7 addr cpuTimeIncrement cpu_time rom48k


applyShifter : Int -> (Int -> FlagRegisters -> IntWithFlags) -> Int -> CpuTimeIncrement -> CpuTimeCTime -> Z80ROM -> Z80 -> Z80
applyShifter new_pc shifterFunc addr cpuTimeIncrement cpu_time rom48k z80 =
    let
        value =
            mem addr cpu_time rom48k z80.env.ram

        result =
            z80.flags |> shifterFunc value.value

        env =
            z80.env

        env_1 =
            { env | time = value.time }

        env_2 =
            env_1 |> setMem addr result.value |> addCpuTimeEnvInc cpuTimeIncrement
    in
    { z80 | pc = new_pc, env = env_2, r = z80.r + 1 }


applyTripleChangeDelta : Z80ROM -> TriplePCIncrement -> CpuTimeCTime -> TripleByteChange -> Z80 -> Z80
applyTripleChangeDelta rom48k pc_increment cpu_time z80changeData z80 =
    let
        new_pc =
            case pc_increment of
                IncrementByThree ->
                    Bitwise.and (z80.pc + 3) 0xFFFF

                IncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF

        env =
            z80.env
    in
    case z80changeData of
        NewBCRegister int ->
            { z80
                | main = z80.main |> set_bc_main int
                , pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        NewDERegister int ->
            { z80
                | main = z80.main |> set_de_main int
                , pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        NewHLRegister int ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | hl = int }
                , pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        NewSPRegister int ->
            { z80
                | pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4, sp = int }
                , r = z80.r + 1
            }

        NewPCRegister int ->
            { z80
                | pc = int
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        CallImmediate int ->
            z80 |> z80_call int cpu_time

        NewIXRegister int ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | ix = int }
                , pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        NewIYRegister int ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | iy = int }
                , pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        NewHLIndirect int ->
            let
                main =
                    z80.main

                value =
                    { env | time = cpu_time } |> mem16 int rom48k
            in
            { z80
                | main = { main | hl = value.value }
                , pc = new_pc
                , env = { env | time = value.time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        NewIXIndirect int ->
            let
                main =
                    z80.main

                value =
                    { env | time = cpu_time } |> mem16 int rom48k
            in
            { z80
                | main = { main | ix = value.value }
                , pc = new_pc
                , env = { env | time = value.time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        NewIYIndirect int ->
            let
                main =
                    z80.main

                value =
                    { env | time = cpu_time } |> mem16 int rom48k
            in
            { z80
                | main = { main | iy = value.value }
                , pc = new_pc
                , env = { env | time = value.time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }


z80_call : Int -> CpuTimeCTime -> Z80 -> Z80
z80_call addr cpu_time z80 =
    let
        new_pc =
            Bitwise.and (z80.pc + 3) 0xFFFF

        env_1 =
            z80.env |> z80_push new_pc
    in
    { z80 | pc = addr, env = { env_1 | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, r = z80.r + 1 }


applyTripleFlagChange : CpuTimeCTime -> TripleWithFlagsChange -> Z80 -> Z80
applyTripleFlagChange cpu_time z80changeData z80 =
    let
        env =
            z80.env
    in
    case z80changeData of
        Skip3ByteInstruction ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 3) 0xFFFF
            in
            { z80
                | pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        AbsoluteJump int ->
            { z80
                | pc = int
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , r = z80.r + 1
            }

        TripleSetIndirect addr value cpuTimeIncrement ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 3) 0xFFFF

                env1 =
                    { env | time = cpu_time } |> setMem addr value |> addCpuTimeEnvInc cpuTimeIncrement4 |> addCpuTimeEnvInc cpuTimeIncrement
            in
            { z80
                | pc = new_pc
                , env = env1
                , r = z80.r + 1
            }

        AbsoluteCall int ->
            z80 |> z80_call int cpu_time
