module Z80Execute exposing (..)

import Bitwise exposing (shiftLeftBy)
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement(..), InstructionDuration(..), addCpuTimeTime, addDuration)
import DoubleWithRegisters exposing (DoubleWithRegisterChange, applyDoubleWithRegistersDelta)
import PCIncrement exposing (InterruptPCIncrement(..), MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import RegisterChange exposing (ChangeMainRegister(..), ChangeOneRegister(..), InterruptChange(..), RegisterChange(..), Shifter(..))
import SingleByteWithEnv exposing (SingleByteEnvChange(..), applyEnvChangeDelta)
import SingleEnvWithMain exposing (EightBitMain(..), SingleEnvMainChange, applySingleEnvMainChange)
import SingleNoParams exposing (NoParamChange(..), applyNoParamsDelta)
import SingleWith8BitParameter exposing (JumpChange(..), Single8BitChange, applySimple8BitChange)
import TripleByte exposing (TripleByteChange(..), TripleByteRegister(..))
import TripleWithFlags exposing (TripleWithFlagsChange(..))
import TripleWithMain exposing (TripleMainChange, applyTripleMainChange)
import Utils exposing (bitMaskFromBit, clearBit, inverseBitMaskFromBit, setBit, shiftLeftBy8, toHexString2)
import Z80Change exposing (FlagChange(..), Z80Change, applyZ80Change)
import Z80Core exposing (Z80Core)
import Z80Debug exposing (debugLog, debugTodo)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..), applyDeltaWithChanges)
import Z80Env exposing (Z80Env, Z80EnvWithPC, mem, mem16, setMem, setMemIgnoringTime, z80_pop, z80_push)
import Z80Flags exposing (FlagRegisters, IntWithFlags, changeFlags, dec, inc, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (InterruptRegisters, MainWithIndexRegisters, get_xy, set_bc_main, set_de_main, set_xy)


type DeltaVariant
    = OldDeltaWithChanges DeltaWithChangesData
    | PureDelta PCIncrement Z80Change
    | InterruptDelta InterruptPCIncrement InterruptChange
    | FlagDelta PCIncrement FlagChange
    | RegisterChangeDelta PCIncrement RegisterChange
    | Simple8BitDelta MediumPCIncrement Single8BitChange
    | DoubleWithRegistersDelta MediumPCIncrement DoubleWithRegisterChange
    | JumpChangeDelta JumpChange
    | NoParamsDelta NoParamChange
    | SingleEnvDelta SingleByteEnvChange
    | MainWithEnvDelta PCIncrement SingleEnvMainChange
    | TripleMainChangeDelta TriplePCIncrement TripleMainChange
    | Triple16ParamDelta TriplePCIncrement TripleByteChange
    | Triple16FlagsDelta TripleWithFlagsChange
    | UnknownInstruction String Int


type alias DeltaWithChanges =
    { deltaVariant : DeltaVariant
    , newTime : CpuTimeCTime
    }


apply_delta : Z80Core -> Z80ROM -> DeltaWithChanges -> Z80Core
apply_delta z80 rom48k z80delta =
    case z80delta.deltaVariant of
        OldDeltaWithChanges deltaWithChangesData ->
            z80 |> applyDeltaWithChanges deltaWithChangesData z80delta.newTime

        PureDelta cpuInc z80ChangeData ->
            z80 |> applyPureDelta cpuInc z80delta.newTime z80ChangeData

        FlagDelta pcInc flagRegisters ->
            z80 |> applyFlagDelta pcInc flagRegisters rom48k z80delta.newTime

        RegisterChangeDelta pcInc registerChange ->
            z80 |> applyRegisterDelta pcInc registerChange rom48k z80delta.newTime

        Simple8BitDelta pcInc single8BitChange ->
            z80 |> applySimple8BitDelta pcInc z80delta.newTime single8BitChange

        DoubleWithRegistersDelta pcInc doubleWithRegisterChange ->
            z80 |> applyDoubleWithRegistersDelta pcInc z80delta.newTime doubleWithRegisterChange rom48k

        JumpChangeDelta jumpChange ->
            z80 |> applyJumpChangeDelta z80delta.newTime jumpChange

        NoParamsDelta noParamChange ->
            z80 |> applyNoParamsDelta z80delta.newTime noParamChange rom48k

        SingleEnvDelta singleByteEnvChange ->
            z80 |> applyEnvChangeDelta z80delta.newTime singleByteEnvChange

        MainWithEnvDelta pcInc singleEnvMainChange ->
            z80 |> applySingleEnvMainChange pcInc z80delta.newTime singleEnvMainChange rom48k

        TripleMainChangeDelta triplePCIncrement tripleMainChange ->
            z80 |> applyTripleMainChange z80delta.newTime triplePCIncrement tripleMainChange

        Triple16ParamDelta triplePCIncrement tripleByteChange ->
            z80 |> applyTripleChangeDelta rom48k triplePCIncrement z80delta.newTime tripleByteChange

        Triple16FlagsDelta tripleWithFlagsChange ->
            z80 |> applyTripleFlagChange z80delta.newTime tripleWithFlagsChange

        UnknownInstruction string int ->
            debugTodo string (int |> toHexString2) z80

        InterruptDelta pCIncrement interruptChange ->
            z80 |> applyInterruptChange pCIncrement interruptChange z80delta.newTime


applyJumpChangeDelta : CpuTimeCTime -> JumpChange -> Z80Core -> Z80Core
applyJumpChangeDelta cpu_time z80changeData z80 =
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


applyInterruptChange : InterruptPCIncrement -> InterruptChange -> CpuTimeCTime -> Z80Core -> Z80Core
applyInterruptChange pcInc change newTime z80 =
    let
        new_pc =
            case pcInc of
                AddTwoToPC ->
                    (z80.pc + 2) |> Bitwise.and 0xFFFF
    in
    case change of
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


applyFlagDelta : PCIncrement -> FlagChange -> Z80ROM -> CpuTimeCTime -> Z80Core -> Z80Core
applyFlagDelta pcInc z80_flags rom48k newTime z80_core =
    let
        new_pc =
            case pcInc of
                IncrementByOne ->
                    (z80_core.pc + 1) |> Bitwise.and 0xFFFF

                IncrementByTwo ->
                    (z80_core.pc + 2) |> Bitwise.and 0xFFFF

                PCIncrementByFour ->
                    Bitwise.and (z80_core.pc + 4) 0xFFFF
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

                PCIncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF
    in
    { z80 | pc = new_pc, clockTime = cpu_time } |> applyZ80Change z80changeData


applyRegisterDelta : PCIncrement -> RegisterChange -> Z80ROM -> CpuTimeCTime -> Z80Core -> Z80Core
applyRegisterDelta pc_inc z80changeData rom48k newTime z80_core =
    let
        env =
            z80_core.env

        new_pc =
            case pc_inc of
                IncrementByOne ->
                    Bitwise.and (z80_core.pc + 1) 0xFFFF

                IncrementByTwo ->
                    Bitwise.and (z80_core.pc + 2) 0xFFFF

                PCIncrementByFour ->
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
                env_2 =
                    env |> setMemIgnoringTime addr value newTime
            in
            { z80_core | pc = new_pc, env = env_2, clockTime = newTime }

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

                --pcWithEnv =
                --    z80 |> z80_call int cpu_time
            in
            --{ z80 | clockTime = cpu_time |> addDuration SevenTStates, pc = pcWithEnv.pc, env = pcWithEnv.env }
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



--z80_call : Int -> CpuTimeCTime -> Z80Core -> Z80EnvWithPC
--z80_call addr cpuTime z80 =
--    let
--        new_pc =
--            Bitwise.and (z80.pc + 3) 0xFFFF
--
--        env_1 =
--            z80.env |> z80_push new_pc cpuTime
--    in
--    { pc = addr, env = env_1 }


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

                --pcWithEnv =
                --    z80 |> z80_call address cpu_time
                env_1 =
                    z80.env |> z80_push new_pc cpu_time
            in
            --{ z80 | clockTime = cpu_time |> addDuration SevenTStates, pc = pcWithEnv.pc, env = pcWithEnv.env }
            { z80 | clockTime = cpu_time |> addDuration SevenTStates, pc = address, env = env_1 }
