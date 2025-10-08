module Z80Execute exposing (..)

import Bitwise exposing (shiftLeftBy)
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement(..), InstructionDuration(..), addCpuTimeTime, addDuration)
import DoubleWithRegisters exposing (DoubleWithRegisterChange, applyDoubleWithRegistersDelta)
import GroupED exposing (inirOtirFlags)
import PCIncrement exposing (InterruptPCIncrement(..), MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import RegisterChange exposing (ChangeMainRegister(..), ChangeOneRegister(..), EDRegisterChange(..), InterruptChange(..), RegisterChange(..), Shifter(..))
import SingleByteWithEnv exposing (SingleByteEnvChange(..), applyEnvChangeDelta)
import SingleEnvWithMain exposing (EightBitMain(..), SingleEnvMainChange, applySingleEnvMainChange)
import SingleNoParams exposing (NoParamChange(..), applyNoParamsDelta)
import SingleWith8BitParameter exposing (JumpChange(..), Single8BitChange, applySimple8BitChange)
import TripleByte exposing (TripleByteChange(..), TripleByteRegister(..))
import TripleWithFlags exposing (TripleWithFlagsChange(..))
import TripleWithMain exposing (TripleMainChange, applyTripleMainChange)
import Utils exposing (bitMaskFromBit, clearBit, inverseBitMaskFromBit, setBit, shiftLeftBy8, toHexString2)
import Z80Change exposing (FlagChange(..), Z80Change(..))
import Z80Core exposing (DirectionForLDIR(..), Z80Core)
import Z80Debug exposing (debugLog, debugTodo)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..), applyDeltaWithChanges)
import Z80Env exposing (Z80Env, Z80EnvWithPC, mem, mem16, setMem, setMemIgnoringTime, z80_in, z80_out, z80_pop, z80_push)
import Z80Flags exposing (FlagRegisters, IntWithFlags, changeFlags, dec, f_szh0n0p, inc, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (InterruptRegisters, MainWithIndexRegisters, get_bc, get_xy, set_bc_main, set_de_main, set_xy)


type DeltaWithChanges
    = OldDeltaWithChanges DeltaWithChangesData
    | PureDelta PCIncrement CpuTimeCTime Z80Change
    | InterruptDelta InterruptPCIncrement InstructionDuration InterruptChange
    | FlagDelta PCIncrement InstructionDuration FlagChange
    | RegisterChangeDelta PCIncrement InstructionDuration RegisterChange
    | EDChangeDelta PCIncrement InstructionDuration EDRegisterChange
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
            z80 |> applyJumpChangeDelta cpuTimeCTime jumpChange rom48k

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

        InterruptDelta pCIncrement duration interruptChange ->
            z80 |> applyInterruptChange pCIncrement duration interruptChange

        EDChangeDelta pCIncrement instructionDuration eDRegisterChange ->
            z80 |> applyEdRegisterDelta pCIncrement instructionDuration eDRegisterChange rom48k


applyJumpChangeDelta : CpuTimeCTime -> JumpChange -> Z80ROM -> Z80Core -> Z80Core
applyJumpChangeDelta cpu_time z80changeData rom48k z80 =
    case z80changeData of
        ActualJump jump ->
            let
                pc =
                    Bitwise.and (z80.pc + 2 + jump) 0xFFFF
            in
            { z80
                | pc = pc
                , clockTime = cpu_time |> addCpuTimeTime 5
            }

        NoJump ->
            let
                pc =
                    Bitwise.and (z80.pc + 2) 0xFFFF
            in
            { z80
                | pc = pc
                , clockTime = cpu_time
            }

        FlagJump flags ->
            let
                pc =
                    Bitwise.and (z80.pc + 2) 0xFFFF
            in
            { z80
                | pc = pc
                , flags = flags
                , clockTime = cpu_time
            }

        Z80Out portNum value ->
            let
                pc =
                    Bitwise.and (z80.pc + 2) 0xFFFF

                ( env, newTime ) =
                    z80.env |> z80_out portNum value cpu_time
            in
            { z80
                | pc = pc
                , env = env
                , clockTime = newTime
            }

        Z80In portNum ->
            let
                pc =
                    Bitwise.and (z80.pc + 2) 0xFFFF

                new_a =
                    z80.env |> z80_in portNum rom48k.keyboard cpu_time

                flags =
                    z80.flags
            in
            { z80
                | pc = pc
                , flags = { flags | a = new_a.value }
                , clockTime = new_a.time
            }


applySimple8BitDelta : MediumPCIncrement -> CpuTimeCTime -> Single8BitChange -> Z80Core -> Z80Core
applySimple8BitDelta pcInc cpu_time z80changeData z80 =
    let
        new_pc =
            case pcInc of
                IncreaseByTwo ->
                    Bitwise.and (z80.pc + 2) 0xFFFF

                IncreaseByThree ->
                    Bitwise.and (z80.pc + 3) 0xFFFF

        main =
            z80.main |> applySimple8BitChange z80changeData
    in
    { z80 | pc = new_pc, main = main, clockTime = cpu_time }


applyInterruptChange : InterruptPCIncrement -> InstructionDuration -> InterruptChange -> Z80Core -> Z80Core
applyInterruptChange pcInc duration chaange z80 =
    let
        new_pc =
            case pcInc of
                AddTwoToPC ->
                    (z80.pc + 2) |> Bitwise.and 0xFFFF

        newTime =
            z80.clockTime |> addDuration duration
    in
    case chaange of
        LoadAFromIR value ->
            --private void ld_a_ir(int v)
            --{
            --	Ff = Ff&~0xFF | (A = v);
            --	Fr = v==0 ? 0 : 1;
            --	Fa = Fb = IFF<<6 & 0x80;
            --	time++;
            --}
            let
                z80_flags =
                    z80.flags

                ff =
                    z80_flags.ff |> Bitwise.and (Bitwise.complement 0xFF) |> Bitwise.or value

                fr =
                    if value == 0 then
                        0

                    else
                        1

                fab =
                    z80.interrupts.iff |> shiftLeftBy 6 |> Bitwise.and 0x80

                flags =
                    { z80_flags | a = value, ff = ff, fr = fr, fa = fab, fb = fab }
            in
            { z80 | pc = new_pc, flags = flags, clockTime = newTime }


applyFlagDelta : PCIncrement -> InstructionDuration -> FlagChange -> Z80ROM -> Z80Core -> Z80Core
applyFlagDelta pcInc duration z80_flags rom48k z80_core =
    let
        new_pc =
            case pcInc of
                IncrementByOne ->
                    (z80_core.pc + 1) |> Bitwise.and 0xFFFF

                IncrementByTwo ->
                    (z80_core.pc + 2) |> Bitwise.and 0xFFFF

                IncrementByThree ->
                    Bitwise.and (z80_core.pc + 3) 0xFFFF

                IncrementByFour ->
                    Bitwise.and (z80_core.pc + 4) 0xFFFF

        newTime =
            z80_core.clockTime |> addDuration duration
    in
    case z80_flags of
        OnlyFlags flagRegisters ->
            { z80_core | pc = new_pc, clockTime = newTime, flags = flagRegisters }

        FlagChange8Bit register value ->
            let
                main =
                    z80_core.main
            in
            case register of
                RegisterB ->
                    { z80_core | pc = new_pc, clockTime = newTime, main = { main | b = value } }

                RegisterC ->
                    { z80_core | pc = new_pc, clockTime = newTime, main = { main | c = value } }

                RegisterD ->
                    { z80_core | pc = new_pc, clockTime = newTime, main = { main | d = value } }

                RegisterE ->
                    { z80_core | pc = new_pc, clockTime = newTime, main = { main | e = value } }

        FlagChangeH int ->
            let
                main =
                    z80_core.main
            in
            { z80_core | pc = new_pc, clockTime = newTime, main = { main | hl = Bitwise.or (shiftLeftBy8 int) (Bitwise.and main.hl 0xFF) } }

        FlagChangeL int ->
            let
                main =
                    z80_core.main
            in
            { z80_core | pc = new_pc, clockTime = newTime, main = { main | hl = Bitwise.or int (Bitwise.and main.hl 0xFF00) } }

        ReturnWithPop ->
            let
                result =
                    z80_core.env |> z80_pop rom48k newTime

                env1 =
                    z80_core.env

                --x = debug_log "ret nz" (result.value |> subName) Nothing
            in
            { z80_core | pc = result.value16, clockTime = result.time, env = { env1 | sp = result.sp } }

        EmptyFlagChange ->
            { z80_core | pc = new_pc, clockTime = newTime }

        FlagNewRValue int ->
            let
                ints =
                    z80_core.interrupts
            in
            { z80_core | pc = new_pc, clockTime = newTime, interrupts = { ints | r = int } }

        FlagChangePush int ->
            { z80_core | pc = new_pc, clockTime = newTime, env = z80_core.env |> z80_push int newTime }


applyPureDelta : PCIncrement -> CpuTimeCTime -> Z80Change -> Z80Core -> Z80Core
applyPureDelta cpuInc cpu_time z80changeData z80 =
    let
        new_pc =
            case cpuInc of
                IncrementByOne ->
                    (z80.pc + 1) |> Bitwise.and 0xFFFF

                IncrementByTwo ->
                    (z80.pc + 2) |> Bitwise.and 0xFFFF

                IncrementByThree ->
                    Bitwise.and (z80.pc + 3) 0xFFFF

                IncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF
    in
    case z80changeData of
        FlagsWithHLRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | pc = new_pc, clockTime = cpu_time, flags = flagRegisters, main = { main | hl = int } }

        Z80ChangeFlags flagRegisters ->
            { z80 | pc = new_pc, clockTime = cpu_time, flags = flagRegisters }

        Z80ChangeSetIndirect addr int ->
            let
                ( env, clockTime ) =
                    z80.env |> setMem addr int cpu_time
            in
            { z80 | pc = new_pc, env = env, clockTime = clockTime }

        FlagsWithIXRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | pc = new_pc, clockTime = cpu_time, flags = flagRegisters, main = { main | ix = int } }

        FlagsWithIYRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | pc = new_pc, clockTime = cpu_time, flags = flagRegisters, main = { main | iy = int } }

        JustIXRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | pc = new_pc, clockTime = cpu_time, main = { main | ix = int } }

        JustIYRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | pc = new_pc, clockTime = cpu_time, main = { main | iy = int } }

        FlagsWithRegisterChange changeMainRegister intWithFlags ->
            let
                main =
                    z80.main

                main2 =
                    case changeMainRegister of
                        ChangeMainB ->
                            { main | b = intWithFlags.value }

                        ChangeMainC ->
                            { main | c = intWithFlags.value }

                        ChangeMainD ->
                            { main | d = intWithFlags.value }

                        ChangeMainE ->
                            { main | e = intWithFlags.value }

                        ChangeMainH ->
                            { main | hl = Bitwise.or (intWithFlags.value |> shiftLeftBy8) (Bitwise.and main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or intWithFlags.value (Bitwise.and main.hl 0xFF00) }
            in
            { z80 | pc = new_pc, clockTime = cpu_time, flags = intWithFlags.flags, main = main2 }


applyRegisterDelta : PCIncrement -> InstructionDuration -> RegisterChange -> Z80ROM -> Z80Core -> Z80Core
applyRegisterDelta pc_inc duration z80changeData rom48k z80_core =
    let
        env =
            z80_core.env

        newTime =
            z80_core.clockTime |> addDuration duration

        new_pc =
            case pc_inc of
                IncrementByOne ->
                    Bitwise.and (z80_core.pc + 1) 0xFFFF

                IncrementByTwo ->
                    Bitwise.and (z80_core.pc + 2) 0xFFFF

                IncrementByThree ->
                    Bitwise.and (z80_core.pc + 3) 0xFFFF

                IncrementByFour ->
                    Bitwise.and (z80_core.pc + 4) 0xFFFF
    in
    case z80changeData of
        ChangeRegisterBC reg_b reg_c ->
            let
                z80_main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { z80_main | b = reg_b, c = reg_c }
                , clockTime = newTime
            }

        ChangeRegisterDE reg_d reg_e ->
            let
                z80_main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { z80_main | d = reg_d, e = reg_e }
                , clockTime = newTime
            }

        ChangeRegisterHL int ->
            let
                z80_main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { z80_main | hl = int }
                , clockTime = newTime
            }

        ChangeRegisterIX int ->
            let
                z80_main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { z80_main | ix = int }
                , clockTime = newTime
            }

        ChangeRegisterIY int ->
            let
                z80_main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { z80_main | iy = int }
                , clockTime = newTime
            }

        ChangeRegisterIXH int ->
            let
                main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF) (int |> shiftLeftBy8) }
                , clockTime = newTime
            }

        ChangeRegisterIXL int ->
            let
                main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF00) int }
                , clockTime = newTime
            }

        ChangeRegisterIYH int ->
            let
                main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { main | iy = Bitwise.or (Bitwise.and main.iy 0xFF) (int |> shiftLeftBy8) }
                , clockTime = newTime
            }

        ChangeRegisterIYL int ->
            let
                main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { main | iy = Bitwise.or (Bitwise.and main.iy 0xFF00) int }
                , clockTime = newTime
            }

        PushedValue int ->
            { z80_core
                | pc = new_pc
                , clockTime = newTime
                , env = z80_core.env |> z80_push int newTime
            }

        RegChangeNewSP int ->
            { z80_core
                | pc = new_pc
                , clockTime = newTime
                , env = { env | sp = int }
            }

        IncrementIndirect addr ->
            -- This should be a primitive operation on Z80Env to increment a stored value
            let
                value =
                    z80_core.env |> mem addr newTime rom48k

                flags =
                    z80_core.flags |> inc value.value

                env_3 =
                    env |> setMemIgnoringTime addr flags.value value.time
            in
            { z80_core | pc = new_pc, env = env_3, flags = flags.flags, clockTime = newTime }

        DecrementIndirect addr ->
            -- This should be a primitive operation on Z80Env to decrement a stored value
            let
                value =
                    z80_core.env |> mem addr newTime rom48k

                flags =
                    z80_core.flags |> dec value.value

                env_3 =
                    env |> setMemIgnoringTime addr flags.value newTime
            in
            { z80_core | pc = new_pc, env = env_3, flags = flags.flags, clockTime = newTime }

        RegisterChangeJump int ->
            { z80_core
                | pc = int
                , clockTime = newTime
            }

        SetIndirect addr value ->
            let
                ( env_2, clockTime ) =
                    env |> setMem addr value newTime
            in
            { z80_core | pc = new_pc, env = env_2, clockTime = clockTime }

        ChangeRegisterDEAndHL de hl ->
            let
                main =
                    z80_core.main
            in
            { z80_core
                | pc = new_pc
                , main = { main | hl = hl } |> set_de_main de
                , clockTime = newTime
            }

        RegisterChangeShifter shifter addr ->
            z80_core |> applyShifter new_pc shifter addr newTime rom48k

        RegisterChangeIndexShifter shifter raw_addr ->
            z80_core |> applyShifter new_pc shifter (raw_addr |> Bitwise.and 0xFFFF) newTime rom48k

        IndirectBitReset bitMask addr ->
            let
                value =
                    env |> mem addr newTime rom48k

                new_value =
                    bitMask |> inverseBitMaskFromBit |> Bitwise.and value.value

                env_3 =
                    env |> setMemIgnoringTime addr new_value value.time
            in
            { z80_core | pc = new_pc, env = env_3, clockTime = newTime }

        IndirectBitSet bitMask raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                value =
                    z80_core.env |> mem addr newTime rom48k

                new_value =
                    bitMask |> bitMaskFromBit |> Bitwise.or value.value

                env_3 =
                    env |> setMemIgnoringTime addr new_value value.time
            in
            { z80_core | pc = new_pc, env = env_3, clockTime = newTime }

        RegChangeNoOp ->
            { z80_core | pc = new_pc, clockTime = newTime }

        SingleEnvFlagFunc flagFunc value ->
            let
                z80_flags =
                    z80_core.flags
            in
            { z80_core
                | pc = new_pc
                , flags = z80_flags |> changeFlags flagFunc value
                , clockTime = newTime
            }

        ExchangeTopOfStackWith ixiyhl ->
            let
                popped =
                    env |> z80_pop rom48k z80_core.clockTime

                xy =
                    z80_core.main |> get_xy ixiyhl

                env_2 =
                    { env | sp = popped.sp } |> z80_push xy popped.time

                main =
                    z80_core.main |> set_xy popped.value16 ixiyhl
            in
            { z80_core | pc = new_pc, env = env_2, main = main, clockTime = newTime }

        SingleRegisterChange changeOneRegister int ->
            let
                z80_main =
                    z80_core.main
            in
            case changeOneRegister of
                ChangeBRegister ->
                    { z80_core
                        | pc = new_pc
                        , main = { z80_main | b = int }
                        , clockTime = newTime
                    }

                ChangeCRegister ->
                    { z80_core
                        | pc = new_pc
                        , main = { z80_main | c = int }
                        , clockTime = newTime
                    }

                ChangeARegister ->
                    let
                        z80_flags =
                            z80_core.flags
                    in
                    { z80_core
                        | pc = new_pc
                        , flags = { z80_flags | a = int }
                        , clockTime = newTime
                    }

                ChangeDRegister ->
                    { z80_core
                        | pc = new_pc
                        , main = { z80_main | d = int }
                        , clockTime = newTime
                    }

                ChangeERegister ->
                    { z80_core
                        | pc = new_pc
                        , main = { z80_main | e = int }
                        , clockTime = newTime
                    }

                ChangeHRegister ->
                    { z80_core
                        | pc = new_pc
                        , main = { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 int) }
                        , clockTime = newTime
                    }

                ChangeLRegister ->
                    { z80_core
                        | pc = new_pc
                        , main = { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) int }
                        , clockTime = newTime
                    }

        RegisterIndirectWithShifter shifterFunc changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    z80_core.env |> mem addr newTime rom48k

                value =
                    case shifterFunc of
                        Shifter0 ->
                            shifter0 input.value z80_core.flags

                        Shifter1 ->
                            shifter1 input.value z80_core.flags

                        Shifter2 ->
                            shifter2 input.value z80_core.flags

                        Shifter3 ->
                            shifter3 input.value z80_core.flags

                        Shifter4 ->
                            shifter4 input.value z80_core.flags

                        Shifter5 ->
                            shifter5 input.value z80_core.flags

                        Shifter6 ->
                            shifter6 input.value z80_core.flags

                        Shifter7 ->
                            shifter7 input.value z80_core.flags

                main =
                    z80_core.main

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
                            { main | hl = Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_core.main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or value.value (Bitwise.and z80_core.main.hl 0xFF00) }

                env_2 =
                    env |> setMemIgnoringTime addr value.value input.time
            in
            { z80_core | pc = new_pc, main = new_main, flags = value.flags, env = env_2, clockTime = input.time }

        SetBitIndirectWithCopy bitTest changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    env |> mem addr newTime rom48k

                value =
                    input.value |> setBit bitTest

                main =
                    z80_core.main

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
                            { main | hl = Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_core.main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or value (Bitwise.and z80_core.main.hl 0xFF00) }

                ( env_2, newTime2 ) =
                    env |> setMem addr value input.time
            in
            { z80_core | pc = new_pc, main = new_main, env = env_2, clockTime = newTime2 }

        ResetBitIndirectWithCopy bitTest changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    env |> mem addr newTime rom48k

                value =
                    input.value |> clearBit bitTest

                main =
                    z80_core.main

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
                            { main | hl = Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_core.main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or value (Bitwise.and z80_core.main.hl 0xFF00) }

                ( env_2, newTme2 ) =
                    env |> setMem addr value input.time
            in
            { z80_core | pc = new_pc, main = new_main, env = env_2, clockTime = newTme2 }


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

        ( env_2, newTime2 ) =
            env |> setMem addr result.value value.time
    in
    { z80 | pc = new_pc, flags = result.flags, env = env_2, clockTime = newTime2 }


applyTripleChangeDelta : Z80ROM -> TriplePCIncrement -> CpuTimeCTime -> TripleByteChange -> Z80Core -> Z80Core
applyTripleChangeDelta rom48k pc_increment cpu_time z80changeData z80 =
    let
        new_pc =
            case pc_increment of
                TripleIncrementByThree ->
                    Bitwise.and (z80.pc + 3) 0xFFFF

                TripleIncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF

        env =
            z80.env
    in
    case z80changeData of
        NewSPRegister int ->
            { z80
                | pc = new_pc
                , clockTime = cpu_time
                , env = { env | sp = int }
            }

        NewPCRegister int ->
            { z80
                | pc = int
                , clockTime = cpu_time
            }

        CallImmediate int ->
            let
                env_1 =
                    z80.env |> z80_push new_pc cpu_time
            in
            { z80 | clockTime = cpu_time |> addDuration SevenTStates, pc = int, env = env_1 }

        NewIXRegister int ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | ix = int }
                , pc = new_pc
                , clockTime = cpu_time
            }

        NewIYRegister int ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | iy = int }
                , pc = new_pc
                , clockTime = cpu_time
            }

        NewHLIndirect int ->
            let
                main =
                    z80.main

                value =
                    env |> mem16 int rom48k cpu_time
            in
            { z80
                | main = { main | hl = value.value16 }
                , pc = new_pc
                , clockTime = value.time
            }

        NewIXIndirect int ->
            let
                main =
                    z80.main

                value =
                    env |> mem16 int rom48k cpu_time
            in
            { z80
                | main = { main | ix = value.value16 }
                , pc = new_pc
                , clockTime = value.time
            }

        NewIYIndirect int ->
            let
                main =
                    z80.main

                value =
                    env |> mem16 int rom48k cpu_time
            in
            { z80
                | main = { main | iy = value.value16 }
                , pc = new_pc
                , clockTime = value.time
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
                , clockTime = value.time
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
                , clockTime = cpu_time
            }


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
                , clockTime = cpu_time
            }

        AbsoluteJump int ->
            { z80
                | pc = int
                , clockTime = cpu_time
            }

        TripleSetIndirect addr value ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 3) 0xFFFF

                ( env2, time2 ) =
                    env |> setMem addr value cpu_time
            in
            { z80
                | pc = new_pc
                , clockTime = time2
                , env = env2
            }

        AbsoluteCall address ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 3) 0xFFFF

                env_1 =
                    z80.env |> z80_push new_pc cpu_time
            in
            { z80 | clockTime = cpu_time |> addDuration SevenTStates, pc = address, env = env_1 }


applyEdRegisterDelta : PCIncrement -> InstructionDuration -> EDRegisterChange -> Z80ROM -> Z80Core -> Z80Core
applyEdRegisterDelta pc_inc duration z80changeData rom48k z80_core =
    let
        env =
            z80_core.env

        newTime =
            z80_core.clockTime |> addDuration duration

        new_pc =
            case pc_inc of
                IncrementByOne ->
                    Bitwise.and (z80_core.pc + 1) 0xFFFF

                IncrementByTwo ->
                    Bitwise.and (z80_core.pc + 2) 0xFFFF

                IncrementByThree ->
                    Bitwise.and (z80_core.pc + 3) 0xFFFF

                IncrementByFour ->
                    Bitwise.and (z80_core.pc + 4) 0xFFFF
    in
    case z80changeData of
        EDNoOp ->
            { z80_core | pc = new_pc, clockTime = newTime }

        RegChangeIm intMode ->
            let
                interrupts =
                    debugLog "SetInterruptMode" intMode z80_core.interrupts
            in
            { z80_core
                | pc = new_pc
                , clockTime = newTime
                , interrupts = { interrupts | iM = intMode }
            }

        Z80InI direction repeat ->
            let
                main =
                    z80_core.main

                new_b =
                    main.b - 1 |> Bitwise.and 0xFF

                new_hl =
                    case direction of
                        Forwards ->
                            main.hl + 1 |> Bitwise.and 0xFFFF

                        Backwards ->
                            main.hl - 1 |> Bitwise.and 0xFFFF

                in_value =
                    z80_core.env |> z80_in (main |> get_bc) rom48k.keyboard newTime

                ( env_2, newTme2 ) =
                    env |> setMem main.hl in_value.value in_value.time

                pc2 =
                    if repeat && new_b /= 0 then
                        z80_core.pc

                    else
                        new_pc

                new_main =
                    { main | hl = new_hl, b = new_b }

                d_flag =
                    case direction of
                        Forwards ->
                            (new_main |> get_bc) + 1 |> Bitwise.and 0xFFFF

                        Backwards ->
                            (new_main |> get_bc) - 1 |> Bitwise.and 0xFFFF

                flags =
                    z80_core.flags |> inirOtirFlags d_flag (new_main |> get_bc) in_value.value
            in
            { z80_core | env = env_2, pc = pc2, flags = flags, main = new_main, clockTime = newTme2 }

        Z80OutI direction repeat ->
            let
                main =
                    z80_core.main

                new_b =
                    main.b - 1 |> Bitwise.and 0xFF

                new_hl =
                    case direction of
                        Forwards ->
                            main.hl + 1 |> Bitwise.and 0xFFFF

                        Backwards ->
                            main.hl - 1 |> Bitwise.and 0xFFFF

                main_2 =
                    { main | b = new_b }

                outvalue =
                    z80_core.env |> mem main.hl newTime rom48k

                new_bc =
                    main_2 |> get_bc

                ( env2, newTime2 ) =
                    z80_core.env |> z80_out new_bc outvalue.value outvalue.time

                pc2 =
                    if repeat && new_b /= 0 then
                        z80_core.pc

                    else
                        new_pc

                flags =
                    z80_core.flags |> inirOtirFlags new_hl new_bc outvalue.value
            in
            { z80_core | env = env2, flags = flags, pc = pc2, main = { main_2 | hl = new_hl }, clockTime = newTime2 }

        InRC changeMainRegister ->
            let
                z80_main =
                    z80_core.main

                z80_flags =
                    z80_core.flags

                in_value =
                    z80_core.env |> z80_in (z80_main |> get_bc) rom48k.keyboard newTime

                main =
                    case changeMainRegister of
                        ChangeMainB ->
                            { z80_main | b = in_value.value }

                        ChangeMainC ->
                            { z80_main | c = in_value.value }

                        ChangeMainD ->
                            { z80_main | d = in_value.value }

                        ChangeMainE ->
                            { z80_main | e = in_value.value }

                        ChangeMainH ->
                            { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 in_value.value) }

                        ChangeMainL ->
                            { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) in_value.value }
            in
            { z80_core | pc = new_pc, flags = z80_flags |> f_szh0n0p in_value.value, main = main, clockTime = in_value.time }
