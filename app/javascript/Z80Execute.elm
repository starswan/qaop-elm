module Z80Execute exposing (..)

import Bitwise exposing (shiftLeftBy)
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement(..), InstructionDuration(..), addCpuTimeTime, addDuration)
import DoubleWithRegisters exposing (DoubleWithRegisterChange, applyDoubleWithRegistersDelta)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import RegisterChange exposing (ChangeMainRegister(..), ChangeOneRegister(..), RegisterChange(..), Shifter(..))
import SingleByteWithEnv exposing (SingleByteEnvChange(..), applyEnvChangeDelta)
import SingleEnvWithMain exposing (EightBitMain(..), SingleEnvMainChange, applySingleEnvMainChange)
import SingleNoParams exposing (NoParamChange(..), applyNoParamsDelta)
import SingleWith8BitParameter exposing (JumpChange(..), Single8BitChange, applySimple8BitChange)
import TripleByte exposing (TripleByteChange(..), TripleByteRegister(..))
import TripleWithFlags exposing (TripleWithFlagsChange(..))
import TripleWithMain exposing (TripleMainChange, applyTripleMainChange)
import Utils exposing (bitMaskFromBit, clearBit, inverseBitMaskFromBit, setBit, shiftLeftBy8, shiftRightBy8, toHexString2)
import Z80Change exposing (FlagChange(..), Z80Change, applyZ80Change)
import Z80Core exposing (Z80Core)
import Z80Debug exposing (debugLog, debugTodo)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..), applyDeltaWithChanges)
import Z80Env exposing (Z80Env, mem, mem16, setMem, z80_pop, z80_push)
import Z80Flags exposing (FlagRegisters, IntWithFlags, changeFlags, dec, inc, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (InterruptRegisters, MainWithIndexRegisters, get_xy, set_bc_main, set_de_main, set_xy)


type DeltaWithChanges
    = OldDeltaWithChanges DeltaWithChangesData
    | PureDelta PCIncrement CpuTimeCTime Z80Change
    | FlagDelta PCIncrement InstructionDuration FlagChange
    | RegisterChangeDelta PCIncrement InstructionDuration RegisterChange
    | Simple8BitDelta MediumPCIncrement CpuTimeCTime Single8BitChange
    | DoubleWithRegistersDelta MediumPCIncrement CpuTimeCTime DoubleWithRegisterChange
    | JumpChangeDelta CpuTimeCTime JumpChange
    | NoParamsDelta CpuTimeCTime NoParamChange
    | SingleEnvDelta CpuTimeCTime SingleByteEnvChange
    | MainWithEnvDelta PCIncrement InstructionDuration SingleEnvMainChange
    | TripleMainChangeDelta CpuTimeCTime TriplePCIncrement TripleMainChange
    | Triple16ParamDelta CpuTimeCTime TriplePCIncrement TripleByteChange
    | Triple16FlagsDelta CpuTimeCTime TripleWithFlagsChange
    | UnknownInstruction String Int


apply_delta : Z80Core -> Z80ROM -> DeltaWithChanges -> Z80Core
apply_delta z80 rom48k z80delta =
    case z80delta of
        OldDeltaWithChanges deltaWithChangesData ->
            z80 |> applyDeltaWithChanges deltaWithChangesData

        PureDelta cpuInc cpu_time z80ChangeData ->
            z80 |> applyPureDelta cpuInc cpu_time z80ChangeData

        FlagDelta pcInc duration flagRegisters ->
            z80 |> applyFlagDelta pcInc duration flagRegisters rom48k

        RegisterChangeDelta pcInc duration registerChange ->
            z80 |> applyRegisterDelta pcInc duration registerChange rom48k

        Simple8BitDelta pcInc cpuTimeCTime single8BitChange ->
            z80 |> applySimple8BitDelta pcInc cpuTimeCTime single8BitChange

        DoubleWithRegistersDelta pcInc cpuTimeCTime doubleWithRegisterChange ->
            z80 |> applyDoubleWithRegistersDelta pcInc cpuTimeCTime doubleWithRegisterChange rom48k

        JumpChangeDelta cpuTimeCTime jumpChange ->
            z80 |> applyJumpChangeDelta cpuTimeCTime jumpChange

        NoParamsDelta cpuTimeCTime noParamChange ->
            z80 |> applyNoParamsDelta cpuTimeCTime noParamChange rom48k

        SingleEnvDelta cpuTimeCTime singleByteEnvChange ->
            z80 |> applyEnvChangeDelta cpuTimeCTime singleByteEnvChange

        MainWithEnvDelta pcInc duration singleEnvMainChange ->
            z80 |> applySingleEnvMainChange pcInc duration singleEnvMainChange rom48k

        TripleMainChangeDelta cpuTimeCTime triplePCIncrement tripleMainChange ->
            z80 |> applyTripleMainChange cpuTimeCTime triplePCIncrement tripleMainChange

        Triple16ParamDelta cpuTimeCTime triplePCIncrement tripleByteChange ->
            z80 |> applyTripleChangeDelta rom48k triplePCIncrement cpuTimeCTime tripleByteChange

        Triple16FlagsDelta cpuTimeCTime tripleWithFlagsChange ->
            z80 |> applyTripleFlagChange cpuTimeCTime tripleWithFlagsChange

        UnknownInstruction string int ->
            debugTodo string (int |> toHexString2) z80


applyJumpChangeDelta : CpuTimeCTime -> JumpChange -> Z80Core -> Z80Core
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
                , env = { old_env | time = cpu_time |> addCpuTimeTime 5 }
                , r = z80.r + 1
            }

        NoJump ->
            let
                pc =
                    Bitwise.and (z80.pc + 2) 0xFFFF
            in
            { z80
                | pc = pc
                , env = { old_env | time = cpu_time }
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


applySimple8BitDelta : MediumPCIncrement -> CpuTimeCTime -> Single8BitChange -> Z80Core -> Z80Core
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


applyFlagDelta : PCIncrement -> InstructionDuration -> FlagChange -> Z80ROM -> Z80Core -> Z80Core
applyFlagDelta pcInc duration z80_flags rom48k z80 =
    let
        env =
            z80.env

        time =
            env.time

        new_pc =
            case pcInc of
                IncrementByOne ->
                    (z80.pc + 1) |> Bitwise.and 0xFFFF

                IncrementByTwo ->
                    (z80.pc + 2) |> Bitwise.and 0xFFFF

                PCIncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF

        env_1 =
            { env | time = time |> addDuration duration }
    in
    case z80_flags of
        OnlyFlags flagRegisters ->
            { z80 | pc = new_pc, env = env_1, r = z80.r + 1, flags = flagRegisters }

        FlagChange8Bit register value ->
            let
                main =
                    z80.main
            in
            case register of
                RegisterB ->
                    { z80 | pc = new_pc, env = env_1, r = z80.r + 1, main = { main | b = value } }

                RegisterC ->
                    { z80 | pc = new_pc, env = env_1, r = z80.r + 1, main = { main | c = value } }

                RegisterD ->
                    { z80 | pc = new_pc, env = env_1, r = z80.r + 1, main = { main | d = value } }

                RegisterE ->
                    { z80 | pc = new_pc, env = env_1, r = z80.r + 1, main = { main | e = value } }

        FlagChangeH int ->
            let
                main =
                    z80.main
            in
            { z80 | pc = new_pc, env = env_1, r = z80.r + 1, main = { main | hl = Bitwise.or (shiftLeftBy8 int) (Bitwise.and main.hl 0xFF) } }

        FlagChangeL int ->
            let
                main =
                    z80.main
            in
            { z80 | pc = new_pc, env = env_1, r = z80.r + 1, main = { main | hl = Bitwise.or int (Bitwise.and main.hl 0xFF00) } }

        ReturnWithPop ->
            let
                result =
                    z80.env |> z80_pop rom48k

                env1 =
                    z80.env

                --x = debug_log "ret nz" (result.value |> subName) Nothing
            in
            { z80 | pc = result.value16, env = { env1 | time = result.time, sp = result.sp } }

        EmptyFlagChange ->
            { z80 | pc = new_pc, env = env_1, r = z80.r + 1 }

        FlagNewRValue int ->
            { z80 | pc = new_pc, env = env_1, r = int }

        FlagChangePush int ->
            { z80 | pc = new_pc, r = z80.r + 1, env = env_1 |> z80_push int }


applyPureDelta : PCIncrement -> CpuTimeCTime -> Z80Change -> Z80Core -> Z80Core
applyPureDelta cpuInc cpu_time z80changeData tmp_z80 =
    let
        env =
            tmp_z80.env

        z80 =
            { tmp_z80 | env = { env | time = cpu_time } }

        new_pc =
            case cpuInc of
                IncrementByOne ->
                    (z80.pc + 1) |> Bitwise.and 0xFFFF

                IncrementByTwo ->
                    (z80.pc + 2) |> Bitwise.and 0xFFFF

                PCIncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF
    in
    { z80 | pc = new_pc, r = z80.r + 1 } |> applyZ80Change z80changeData


applyRegisterDelta : PCIncrement -> InstructionDuration -> RegisterChange -> Z80ROM -> Z80Core -> Z80Core
applyRegisterDelta pc_inc duration z80changeData rom48k z80 =
    let
        env =
            z80.env

        env_1 =
            { env | time = env.time |> addDuration duration }

        new_pc =
            case pc_inc of
                IncrementByOne ->
                    Bitwise.and (z80.pc + 1) 0xFFFF

                IncrementByTwo ->
                    Bitwise.and (z80.pc + 2) 0xFFFF

                PCIncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF

        new_r =
            z80.r + 1
    in
    case z80changeData of
        ChangeRegisterBC reg_b reg_c ->
            let
                z80_main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { z80_main | b = reg_b, c = reg_c }
                , env = env_1
                , r = new_r
            }

        ChangeRegisterDE reg_d reg_e ->
            let
                z80_main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { z80_main | d = reg_d, e = reg_e }
                , env = env_1
                , r = new_r
            }

        ChangeRegisterHL int ->
            let
                z80_main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { z80_main | hl = int }
                , env = env_1
                , r = new_r
            }

        ChangeRegisterIX int ->
            let
                z80_main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { z80_main | ix = int }
                , env = env_1
                , r = new_r
            }

        ChangeRegisterIY int ->
            let
                z80_main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { z80_main | iy = int }
                , env = env_1
                , r = new_r
            }

        ChangeRegisterIXH int ->
            let
                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF) (int |> shiftLeftBy8) }
                , env = env_1
                , r = new_r
            }

        ChangeRegisterIXL int ->
            let
                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF00) int }
                , env = env_1
                , r = new_r
            }

        ChangeRegisterIYH int ->
            let
                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | iy = Bitwise.or (Bitwise.and main.iy 0xFF) (int |> shiftLeftBy8) }
                , env = env_1
                , r = new_r
            }

        ChangeRegisterIYL int ->
            let
                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | iy = Bitwise.or (Bitwise.and main.iy 0xFF00) int }
                , env = env_1
                , r = new_r
            }

        PushedValue int ->
            { z80
                | pc = new_pc
                , env = env_1 |> z80_push int
                , r = new_r
            }

        RegChangeNewSP int ->
            { z80
                | pc = new_pc
                , env = { env_1 | sp = int }
                , r = new_r
            }

        IncrementIndirect addr ->
            -- This should be a primitive operation on Z80Env to increment a stored value
            let
                value =
                    z80.env |> mem addr env_1.time rom48k

                env_2 =
                    { env | time = value.time }

                flags =
                    z80.flags |> inc value.value

                env_3 =
                    env_2 |> setMem addr flags.value
            in
            { z80 | pc = new_pc, env = env_3, flags = flags.flags, r = new_r }

        DecrementIndirect addr ->
            -- This should be a primitive operation on Z80Env to decrement a stored value
            let
                value =
                    z80.env |> mem addr env_1.time rom48k

                env_2 =
                    { env_1 | time = value.time }

                flags =
                    z80.flags |> dec value.value

                env_3 =
                    env_2 |> setMem addr flags.value
            in
            { z80 | pc = new_pc, env = env_3, flags = flags.flags, r = new_r }

        RegisterChangeJump int ->
            { z80
                | pc = int
                , env = env_1
                , r = new_r
            }

        SetIndirect addr value ->
            let
                env_2 =
                    env_1 |> setMem addr value
            in
            { z80 | pc = new_pc, env = env_2, r = new_r }

        ChangeRegisterDEAndHL de hl ->
            let
                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | hl = hl } |> set_de_main de
                , env = env_1
                , r = new_r
            }

        RegisterChangeShifter shifter addr ->
            z80 |> applyShifter new_pc shifter addr env_1.time rom48k

        RegisterChangeIndexShifter shifter raw_addr ->
            z80 |> applyShifter new_pc shifter (raw_addr |> Bitwise.and 0xFFFF) env_1.time rom48k

        IndirectBitReset bitMask addr ->
            let
                value =
                    z80.env |> mem addr env_1.time rom48k

                env_2 =
                    { env_1 | time = value.time }

                new_value =
                    bitMask |> inverseBitMaskFromBit |> Bitwise.and value.value

                env_3 =
                    env_2 |> setMem addr new_value
            in
            { z80 | pc = new_pc, env = env_3, r = new_r }

        IndirectBitSet bitMask raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                value =
                    z80.env |> mem addr env_1.time rom48k

                env_2 =
                    { env_1 | time = value.time }

                new_value =
                    bitMask |> bitMaskFromBit |> Bitwise.or value.value

                env_3 =
                    env_2 |> setMem addr new_value
            in
            { z80 | pc = new_pc, env = env_3, r = new_r }

        RegChangeNoOp ->
            { z80 | pc = new_pc, env = env_1, r = new_r }

        SingleEnvFlagFunc flagFunc value ->
            let
                z80_flags =
                    z80.flags
            in
            { z80
                | pc = new_pc
                , flags = z80_flags |> changeFlags flagFunc value
                , env = env_1
                , r = new_r
            }

        RegChangeIm intMode ->
            let
                interrupts =
                    debugLog "SetInterruptMode" intMode z80.interrupts
            in
            { z80
                | pc = new_pc
                , env = env_1
                , r = new_r
                , interrupts = { interrupts | iM = intMode }
            }

        ExchangeTopOfStackWith ixiyhl ->
            let
                popped =
                    env_1 |> z80_pop rom48k

                xy =
                    z80.main |> get_xy ixiyhl

                env_2 =
                    { env_1 | sp = popped.sp } |> z80_push xy

                main =
                    z80.main |> set_xy popped.value16 ixiyhl
            in
            { z80 | pc = new_pc, r = new_r, env = env_2, main = main }

        SingleRegisterChange changeOneRegister int ->
            let
                z80_main =
                    z80.main
            in
            case changeOneRegister of
                ChangeBRegister ->
                    { z80
                        | pc = new_pc
                        , main = { z80_main | b = int }
                        , env = env_1
                        , r = new_r
                    }

                ChangeCRegister ->
                    { z80
                        | pc = new_pc
                        , main = { z80_main | c = int }
                        , env = env_1
                        , r = new_r
                    }

                ChangeARegister ->
                    let
                        z80_flags =
                            z80.flags
                    in
                    { z80
                        | pc = new_pc
                        , flags = { z80_flags | a = int }
                        , env = env_1
                        , r = new_r
                    }

                ChangeDRegister ->
                    { z80
                        | pc = new_pc
                        , main = { z80_main | d = int }
                        , env = env_1
                        , r = new_r
                    }

                ChangeERegister ->
                    { z80
                        | pc = new_pc
                        , main = { z80_main | e = int }
                        , env = env_1
                        , r = new_r
                    }

                ChangeHRegister ->
                    { z80
                        | pc = new_pc
                        , main = { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 int) }
                        , env = env_1
                        , r = new_r
                    }

                ChangeLRegister ->
                    { z80
                        | pc = new_pc
                        , main = { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) int }
                        , env = env_1
                        , r = new_r
                    }

        RegisterIndirectWithShifter shifterFunc changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    env_1 |> mem addr z80.env.time rom48k

                value =
                    case shifterFunc of
                        Shifter0 ->
                            shifter0 input.value z80.flags

                        Shifter1 ->
                            shifter1 input.value z80.flags

                        Shifter2 ->
                            shifter2 input.value z80.flags

                        Shifter3 ->
                            shifter3 input.value z80.flags

                        Shifter4 ->
                            shifter4 input.value z80.flags

                        Shifter5 ->
                            shifter5 input.value z80.flags

                        Shifter6 ->
                            shifter6 input.value z80.flags

                        Shifter7 ->
                            shifter7 input.value z80.flags

                main =
                    z80.main

                new_main =
                    case changeOneRegister of
                        ChangeMainB ->
                            { main | b = value.value }

                        ChangeMainC ->
                            { main | c = value.value }

                        ChangeMainD ->
                            { main | d = value.value }

                        ChangeMainE ->
                            { main | e = value.value }

                        ChangeMainH ->
                            { main | hl = Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80.main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or value.value (Bitwise.and z80.main.hl 0xFF00) }

                env_2 =
                    { env_1 | time = input.time } |> setMem addr value.value
            in
            { z80 | pc = new_pc, main = new_main, flags = value.flags, env = env_2, r = z80.r + 1 }

        SetBitIndirectWithCopy bitTest changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    env_1 |> mem addr z80.env.time rom48k

                value =
                    input.value |> setBit bitTest

                main =
                    z80.main

                new_main =
                    case changeOneRegister of
                        ChangeMainB ->
                            { main | b = value }

                        ChangeMainC ->
                            { main | c = value }

                        ChangeMainD ->
                            { main | d = value }

                        ChangeMainE ->
                            { main | e = value }

                        ChangeMainH ->
                            { main | hl = Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80.main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or value (Bitwise.and z80.main.hl 0xFF00) }

                env_2 =
                    { env_1 | time = input.time } |> setMem addr value
            in
            { z80 | pc = new_pc, main = new_main, env = env_2, r = z80.r + 1 }

        ResetBitIndirectWithCopy bitTest changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    env_1 |> mem addr z80.env.time rom48k

                value =
                    input.value |> clearBit bitTest

                main =
                    z80.main

                new_main =
                    case changeOneRegister of
                        ChangeMainB ->
                            { main | b = value }

                        ChangeMainC ->
                            { main | c = value }

                        ChangeMainD ->
                            { main | d = value }

                        ChangeMainE ->
                            { main | e = value }

                        ChangeMainH ->
                            { main | hl = Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80.main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or value (Bitwise.and z80.main.hl 0xFF00) }

                env_2 =
                    { env_1 | time = input.time } |> setMem addr value
            in
            { z80 | pc = new_pc, main = new_main, env = env_2, r = z80.r + 1 }

        LoadAFromI ->
            let
                -- case 0x57: ld_a_ir(IR>>>8); break;
                flags =
                    z80.flags |> ld_a_ir (z80.interrupts.ir |> shiftRightBy8) z80.interrupts
            in
            { z80 | pc = new_pc, r = z80.r + 1, flags = flags }


ld_a_ir : Int -> InterruptRegisters -> FlagRegisters -> FlagRegisters
ld_a_ir v interrupts flags =
    --private void ld_a_ir(int v)
    --{
    --	Ff = Ff&~0xFF | (A = v);
    --	Fr = v==0 ? 0 : 1;
    --	Fa = Fb = IFF<<6 & 0x80;
    --	time++;
    --}
    let
        ff =
            flags.ff |> Bitwise.and (Bitwise.complement 0xFF) |> Bitwise.or v

        fr =
            if v == 0 then
                0

            else
                1

        fab =
            interrupts.iff |> shiftLeftBy 6 |> Bitwise.and 0x80
    in
    { flags | a = v, ff = ff, fr = fr, fa = fab, fb = fab }


applyShifter : Int -> Shifter -> Int -> CpuTimeCTime -> Z80ROM -> Z80Core -> Z80Core
applyShifter new_pc shifterFunc addr cpu_time rom48k z80 =
    let
        value =
            z80.env |> mem addr cpu_time rom48k

        result =
            case shifterFunc of
                Shifter0 ->
                    z80.flags |> shifter0 value.value

                Shifter1 ->
                    z80.flags |> shifter1 value.value

                Shifter2 ->
                    z80.flags |> shifter2 value.value

                Shifter3 ->
                    z80.flags |> shifter3 value.value

                Shifter4 ->
                    z80.flags |> shifter4 value.value

                Shifter5 ->
                    z80.flags |> shifter5 value.value

                Shifter6 ->
                    z80.flags |> shifter6 value.value

                Shifter7 ->
                    z80.flags |> shifter7 value.value

        env =
            z80.env

        env_1 =
            { env | time = value.time }

        env_2 =
            env_1 |> setMem addr result.value
    in
    { z80 | pc = new_pc, flags = result.flags, env = env_2, r = z80.r + 1 }


applyTripleChangeDelta : Z80ROM -> TriplePCIncrement -> CpuTimeCTime -> TripleByteChange -> Z80Core -> Z80Core
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
        NewSPRegister int ->
            { z80
                | pc = new_pc
                , env = { env | time = cpu_time, sp = int }
                , r = z80.r + 1
            }

        NewPCRegister int ->
            { z80
                | pc = int
                , env = { env | time = cpu_time }
                , r = z80.r + 1
            }

        CallImmediate int ->
            z80 |> z80_call int

        NewIXRegister int ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | ix = int }
                , pc = new_pc
                , env = { env | time = cpu_time }
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
                , env = { env | time = cpu_time }
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
                | main = { main | hl = value.value16 }
                , pc = new_pc
                , env = { env | time = value.time }
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
                | main = { main | ix = value.value16 }
                , pc = new_pc
                , env = { env | time = value.time }
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
                | main = { main | iy = value.value16 }
                , pc = new_pc
                , env = { env | time = value.time }
                , r = z80.r + 1
            }

        NewAIndirect int ->
            let
                value =
                    env |> mem int cpu_time rom48k

                flags =
                    z80.flags
            in
            { z80
                | flags = { flags | a = value.value }
                , pc = new_pc
                , env = { env | time = value.time }
                , r = z80.r + 1
            }

        NewTripleRegister int tripleByteRegister ->
            let
                z80_main =
                    case tripleByteRegister of
                        TripleByteBC ->
                            z80.main |> set_bc_main int

                        TripleByteDE ->
                            z80.main |> set_de_main int

                        TripleByteHL ->
                            let
                                main =
                                    z80.main
                            in
                            { main | hl = int }
            in
            { z80
                | main = z80_main
                , pc = new_pc
                , env = { env | time = cpu_time }
                , r = z80.r + 1
            }


z80_call : Int -> Z80Core -> Z80Core
z80_call addr z80 =
    let
        new_pc =
            Bitwise.and (z80.pc + 3) 0xFFFF

        env_1 =
            z80.env |> z80_push new_pc
    in
    { z80 | pc = addr, env = env_1, r = z80.r + 1 }


applyTripleFlagChange : CpuTimeCTime -> TripleWithFlagsChange -> Z80Core -> Z80Core
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
                , env = { env | time = cpu_time }
                , r = z80.r + 1
            }

        AbsoluteJump int ->
            { z80
                | pc = int
                , env = { env | time = cpu_time }
                , r = z80.r + 1
            }

        TripleSetIndirect addr value ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 3) 0xFFFF
            in
            { z80
                | pc = new_pc
                , env = { env | time = cpu_time } |> setMem addr value
                , r = z80.r + 1
            }

        AbsoluteCall int ->
            { z80 | env = { env | time = cpu_time |> addDuration SevenTStates } } |> z80_call int
