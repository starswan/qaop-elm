module Z80Execute exposing (..)

import Bitwise exposing (shiftLeftBy)
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import DoubleWithRegisters exposing (DoubleWithRegisterChange, applyDoubleWithRegistersDelta)
import GroupED exposing (adc_hl_sp, cpir, execute_ED70, execute_ED78, inirOtirFlags, ldir, rld, rrd, sbc_hl)
import RegisterChange exposing (EDFourByteChange(..), EDRegisterChange(..), InterruptChange(..), RegisterChange(..), Shifter(..), SixteenBit(..))
import SingleByteWithEnv exposing (SingleByteEnvChange(..), applyEnvChangeDelta)
import SingleEnvWithMain exposing (SingleEnvMainChange, applySingleEnvMainChange)
import SingleNoParams exposing (NoParamChange(..), RstChange, applyNoParamsDelta, applyRstDelta)
import SingleWith8BitParameter exposing (JumpChange(..), Single8BitChange(..), applySimple8BitChange)
import TripleByte exposing (TripleByteChange(..), TripleByteRegister(..))
import TripleWithFlags exposing (TripleWithFlagsChange(..))
import TripleWithMain exposing (TripleMainChange, applyTripleMainChange)
import Utils exposing (bitMaskFromBit, clearBit, inverseBitMaskFromBit, setBit, shiftLeftBy8, toHexString2)
import Z80Change exposing (FlagChange(..), Z80Change(..))
import Z80Core exposing (CoreChange(..), DirectionForLDIR(..), RepeatPCOffset(..), Z80Core)
import Z80Debug exposing (debugLog, debugTodo)
import Z80Env exposing (Z80Env, Z80EnvWithPC, setMem, setMem16, z80_in, z80_out, z80_push)
import Z80Flags exposing (FlagRegisters, IntWithFlags, changeFlags, dec, f_szh0n0p, inc, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7)
import Z80Mem exposing (mem, mem16, z80_pop)
import Z80Registers exposing (ChangeMainRegister(..), ChangeOneRegister(..), CoreRegister(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), InterruptRegisters, MainWithIndexRegisters, get_bc, get_de, get_xy, set_bc_main, set_de_main, set_xy)


type DeltaWithChanges
    = PureDelta Z80Change
    | InterruptDelta InterruptChange
    | FlagDelta FlagChange
    | RegisterChangeDelta RegisterChange
    | EDChangeDelta EDRegisterChange
    | EDFourByteDelta EDFourByteChange
    | Simple8BitDelta Single8BitChange
    | DoubleWithRegistersDelta DoubleWithRegisterChange
    | JumpChangeDelta JumpChange
    | NoParamsDelta NoParamChange
    | SingleEnvDelta SingleByteEnvChange
    | MainWithEnvDelta SingleEnvMainChange
    | TripleMainChangeDelta CpuTimeCTime TripleMainChange
    | Triple16ParamDelta TripleByteChange
    | Triple16FlagsDelta TripleWithFlagsChange
    | UnknownInstruction String Int
    | RstDelta RstChange


apply_delta : Z80Core -> Z80ROM -> CpuTimeCTime -> DeltaWithChanges -> CoreChange
apply_delta z80 rom48k clockTime z80delta =
    case z80delta of
        PureDelta z80ChangeData ->
            z80 |> applyPureDelta clockTime z80ChangeData |> CoreOnly

        FlagDelta flagRegisters ->
            z80 |> applyFlagDelta clockTime flagRegisters rom48k

        RegisterChangeDelta registerChange ->
            z80 |> applyRegisterDelta clockTime registerChange rom48k

        Simple8BitDelta single8BitChange ->
            z80 |> applySimple8BitDelta clockTime single8BitChange rom48k |> CoreOnly

        DoubleWithRegistersDelta doubleWithRegisterChange ->
            z80 |> applyDoubleWithRegistersDelta clockTime doubleWithRegisterChange rom48k |> CoreOnly

        JumpChangeDelta jumpChange ->
            z80 |> applyJumpChangeDelta jumpChange

        NoParamsDelta noParamChange ->
            z80 |> applyNoParamsDelta clockTime noParamChange rom48k

        SingleEnvDelta singleByteEnvChange ->
            z80 |> applyEnvChangeDelta singleByteEnvChange |> CoreOnly

        MainWithEnvDelta singleEnvMainChange ->
            z80 |> applySingleEnvMainChange clockTime singleEnvMainChange rom48k

        TripleMainChangeDelta cpuTimeCTime tripleMainChange ->
            let
                ( newz80, clocktime ) =
                    z80 |> applyTripleMainChange cpuTimeCTime tripleMainChange
            in
            newz80 |> CoreOnly

        Triple16ParamDelta tripleByteChange ->
            z80 |> applyTripleChangeDelta rom48k clockTime tripleByteChange

        Triple16FlagsDelta tripleWithFlagsChange ->
            z80 |> applyTripleFlagChange tripleWithFlagsChange

        UnknownInstruction string int ->
            debugTodo string (int |> toHexString2) z80 |> CoreOnly

        InterruptDelta interruptChange ->
            z80 |> applyInterruptChange interruptChange |> CoreOnly

        EDChangeDelta eDRegisterChange ->
            z80 |> applyEdRegisterDelta clockTime eDRegisterChange rom48k

        EDFourByteDelta fourByteChnage ->
            z80 |> applyEdFourByte clockTime fourByteChnage rom48k

        RstDelta noParamChange ->
            z80 |> applyRstDelta clockTime noParamChange


applyJumpChangeDelta : JumpChange -> Z80Core -> CoreChange
applyJumpChangeDelta z80changeData z80 =
    case z80changeData of
        ActualJumpOffset offset ->
            JumpWithOffset offset

        ConditionalJumpOffset offset shortDelay function ->
            if z80.flags |> function then
                JumpOffsetWithDelay offset shortDelay

            else
                NoCore

        DJNZOffset offset shortDelay ->
            let
                --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
                --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
                --PC=(char)v;} break;
                b =
                    Bitwise.and (z80.main.b - 1) 0xFF

                main =
                    z80.main
            in
            if b /= 0 then
                { z80 | main = { main | b = b } }
                    |> CoreWithOffsetAndDelay offset shortDelay

            else
                { z80 | main = { main | b = b } }
                    |> CoreOnly


applySimple8BitDelta : CpuTimeCTime -> Single8BitChange -> Z80ROM -> Z80Core -> Z80Core
applySimple8BitDelta cpu_time z80changeData rom48k z80 =
    case z80changeData of
        NewRegister coreRegister int ->
            let
                main =
                    z80.main |> applySimple8BitChange coreRegister int
            in
            { z80 | main = main }

        NewARegister new_a ->
            let
                flags =
                    z80.flags
            in
            { z80
                | flags = { flags | a = new_a }
            }

        FlagJump operation param ->
            let
                flags =
                    z80.flags |> changeFlags operation param
            in
            { z80 | flags = flags }

        Z80In param ->
            -- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
            let
                flags =
                    z80.flags

                portNum =
                    Bitwise.or param (shiftLeftBy8 flags.a)

                new_a =
                    z80.env |> z80_in portNum rom48k.keyboard cpu_time
            in
            { z80 | flags = { flags | a = new_a.value } }

        Z80Out param ->
            let
                -- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
                portNum =
                    Bitwise.or param (shiftLeftBy8 z80.flags.a)

                ( env, newTime ) =
                    z80.env |> z80_out portNum z80.flags.a cpu_time
            in
            { z80 | env = env }


applyInterruptChange : InterruptChange -> Z80Core -> Z80Core
applyInterruptChange chaange z80 =
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
            { z80 | flags = flags }


applyFlagDelta : CpuTimeCTime -> FlagChange -> Z80ROM -> Z80Core -> CoreChange
applyFlagDelta clockTime z80_flags rom48k z80_core =
    case z80_flags of
        OnlyFlags flagRegisters ->
            { z80_core | flags = flagRegisters } |> CoreOnly

        FlagChange8Bit register value ->
            let
                main =
                    z80_core.main
            in
            case register of
                RegisterB ->
                    { z80_core | main = { main | b = value } } |> CoreOnly

                RegisterC ->
                    { z80_core | main = { main | c = value } } |> CoreOnly

                RegisterD ->
                    { z80_core | main = { main | d = value } } |> CoreOnly

                RegisterE ->
                    { z80_core | main = { main | e = value } } |> CoreOnly

        FlagChangeH int ->
            let
                main =
                    z80_core.main
            in
            { z80_core | main = { main | hl = Bitwise.or (shiftLeftBy8 int) (Bitwise.and main.hl 0xFF) } } |> CoreOnly

        FlagChangeL int ->
            let
                main =
                    z80_core.main
            in
            { z80_core | main = { main | hl = Bitwise.or int (Bitwise.and main.hl 0xFF00) } } |> CoreOnly

        ReturnWithPop ->
            let
                result =
                    z80_core.env |> z80_pop rom48k clockTime

                env1 =
                    z80_core.env
            in
            { z80_core | env = { env1 | sp = result.sp } } |> CoreWithPC result.value16

        EmptyFlagChange ->
            NoCore

        FlagNewRValue int ->
            let
                ints =
                    z80_core.interrupts
            in
            { z80_core | interrupts = { ints | r = int } } |> CoreOnly

        FlagNewIValue int ->
            let
                ints =
                    z80_core.interrupts

                new_ir =
                    ints.ir |> Bitwise.and 0xFF |> Bitwise.or (int |> shiftLeftBy8)
            in
            { z80_core | interrupts = { ints | ir = new_ir } } |> CoreOnly

        FlagChangePush int ->
            { z80_core | env = z80_core.env |> z80_push int clockTime } |> CoreOnly


applyPureDelta : CpuTimeCTime -> Z80Change -> Z80Core -> Z80Core
applyPureDelta clockTime z80changeData z80 =
    case z80changeData of
        FlagsWithHLRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | hl = int } }

        Z80ChangeFlags flagRegisters ->
            { z80 | flags = flagRegisters }

        Z80ChangeSetIndirect addr int ->
            let
                ( env, clockTime2 ) =
                    z80.env |> setMem addr int clockTime
            in
            { z80 | env = env }

        FlagsWithIXRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | ix = int } }

        FlagsWithIYRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | iy = int } }

        JustIXRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | ix = int } }

        JustIYRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | iy = int } }

        FlagsWithRegisterChange changeMainRegister intWithFlags ->
            let
                z80_main =
                    z80.main

                new_main =
                    case changeMainRegister of
                        RegisterB ->
                            { z80_main | b = intWithFlags.value }

                        RegisterC ->
                            { z80_main | c = intWithFlags.value }

                        RegisterD ->
                            { z80_main | d = intWithFlags.value }

                        RegisterE ->
                            { z80_main | e = intWithFlags.value }
            in
            { z80 | flags = intWithFlags.flags, main = new_main }


applyRegisterDelta : CpuTimeCTime -> RegisterChange -> Z80ROM -> Z80Core -> CoreChange
applyRegisterDelta clockTime z80changeData rom48k z80_core =
    let
        env =
            z80_core.env
    in
    case z80changeData of
        ChangeRegisterBC reg_b reg_c ->
            let
                z80_main =
                    z80_core.main
            in
            { z80_core | main = { z80_main | b = reg_b, c = reg_c } }
                |> CoreOnly

        ChangeRegisterDE reg_d reg_e ->
            let
                z80_main =
                    z80_core.main
            in
            { z80_core | main = { z80_main | d = reg_d, e = reg_e } }
                |> CoreOnly

        ChangeRegisterHL ixiyhl int ->
            let
                z80_main =
                    z80_core.main

                main =
                    case ixiyhl of
                        IX ->
                            { z80_main | ix = int }

                        IY ->
                            { z80_main | iy = int }

                        HL ->
                            { z80_main | hl = int }
            in
            { z80_core | main = main } |> CoreOnly

        ChangeRegisterIXH int ->
            let
                main =
                    z80_core.main
            in
            { z80_core | main = { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF) (int |> shiftLeftBy8) } }
                |> CoreOnly

        ChangeRegisterIXL int ->
            let
                main =
                    z80_core.main
            in
            { z80_core | main = { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF00) int } }
                |> CoreOnly

        ChangeRegisterIYH int ->
            let
                main =
                    z80_core.main
            in
            { z80_core | main = { main | iy = Bitwise.or (Bitwise.and main.iy 0xFF) (int |> shiftLeftBy8) } }
                |> CoreOnly

        ChangeRegisterIYL int ->
            let
                main =
                    z80_core.main
            in
            { z80_core | main = { main | iy = Bitwise.or (Bitwise.and main.iy 0xFF00) int } }
                |> CoreOnly

        PushedValue int ->
            { z80_core | env = z80_core.env |> z80_push int clockTime }
                |> CoreOnly

        RegChangeNewSP int ->
            { z80_core | env = { env | sp = int } }
                |> CoreOnly

        IncrementIndirect addr ->
            -- This should be a primitive operation on Z80Env to increment a stored value
            let
                value =
                    z80_core.env |> mem addr clockTime rom48k

                flags =
                    z80_core.flags |> inc value.value

                ( env_3, newNew ) =
                    env |> setMem addr flags.value value.time
            in
            { z80_core | env = env_3, flags = flags.flags } |> CoreOnly

        DecrementIndirect addr ->
            -- This should be a primitive operation on Z80Env to decrement a stored value
            let
                value =
                    z80_core.env |> mem addr clockTime rom48k

                flags =
                    z80_core.flags |> dec value.value

                ( env_3, newNew ) =
                    env |> setMem addr flags.value clockTime
            in
            { z80_core | env = env_3, flags = flags.flags } |> CoreOnly

        RegisterChangeJump int ->
            JumpOnlyPC int

        SetIndirect addr value ->
            let
                ( env_2, newTime ) =
                    env |> setMem addr value clockTime
            in
            { z80_core | env = env_2 } |> CoreOnly

        ChangeRegisterDEAndHL de hl ->
            let
                main =
                    z80_core.main
            in
            { z80_core | main = { main | hl = hl } |> set_de_main de }
                |> CoreOnly

        RegisterChangeShifter shifter addr ->
            z80_core |> applyShifter shifter addr clockTime rom48k |> CoreOnly

        RegisterChangeIndexShifter shifter raw_addr ->
            z80_core |> applyShifter shifter (raw_addr |> Bitwise.and 0xFFFF) clockTime rom48k |> CoreOnly

        IndirectBitReset bitMask addr ->
            let
                value =
                    env |> mem addr clockTime rom48k

                new_value =
                    bitMask |> inverseBitMaskFromBit |> Bitwise.and value.value

                ( env_3, newNewTime ) =
                    env |> setMem addr new_value value.time
            in
            { z80_core | env = env_3 } |> CoreOnly

        IndirectBitSet bitMask raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                value =
                    z80_core.env |> mem addr clockTime rom48k

                new_value =
                    bitMask |> bitMaskFromBit |> Bitwise.or value.value

                ( env_3, newNewTime ) =
                    env |> setMem addr new_value value.time
            in
            { z80_core | env = env_3 } |> CoreOnly

        RegChangeNoOp ->
            NoCore

        SingleEnvFlagFunc flagFunc value ->
            let
                z80_flags =
                    z80_core.flags
            in
            { z80_core | flags = z80_flags |> changeFlags flagFunc value }
                |> CoreOnly

        ExchangeTopOfStackWith ixiyhl ->
            let
                popped =
                    env |> z80_pop rom48k clockTime

                xy =
                    z80_core.main |> get_xy ixiyhl

                env_2 =
                    { env | sp = popped.sp } |> z80_push xy popped.time

                main =
                    z80_core.main |> set_xy popped.value16 ixiyhl
            in
            { z80_core | env = env_2, main = main } |> CoreOnly

        SingleRegisterChange changeOneRegister int ->
            let
                z80_main =
                    z80_core.main

                main =
                    case changeOneRegister of
                        ChangeMainB ->
                            { z80_main | b = int }

                        ChangeMainC ->
                            { z80_main | c = int }

                        ChangeMainD ->
                            { z80_main | d = int }

                        ChangeMainE ->
                            { z80_main | e = int }

                        ChangeMainH ->
                            { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 int) }

                        ChangeMainL ->
                            { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) int }
            in
            { z80_core | main = main } |> CoreOnly

        RegisterChangeA int ->
            let
                z80_flags =
                    z80_core.flags
            in
            { z80_core | flags = { z80_flags | a = int } } |> CoreOnly

        RegisterIndirectWithShifter shifterFunc changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    z80_core.env |> mem addr clockTime rom48k

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

                ( env_2, newNew ) =
                    env |> setMem addr value.value input.time
            in
            { z80_core | main = new_main, flags = value.flags, env = env_2 } |> CoreOnly

        SetBitIndirectWithCopy bitTest changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    env |> mem addr clockTime rom48k

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
            { z80_core | main = new_main, env = env_2 } |> CoreOnly

        ResetBitIndirectWithCopy bitTest changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    env |> mem addr clockTime rom48k

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

                ( env_2, newTime ) =
                    env |> setMem addr value input.time
            in
            { z80_core | main = new_main, env = env_2 } |> CoreOnly

        FlagsIndirectWithShifter shifterFunc raw_addr ->
            let
                address =
                    raw_addr |> Bitwise.and 0xFFFF

                value =
                    z80_core.env |> mem address clockTime rom48k

                result =
                    case shifterFunc of
                        Shifter0 ->
                            z80_core.flags |> shifter0 value.value

                        Shifter1 ->
                            z80_core.flags |> shifter1 value.value

                        Shifter2 ->
                            z80_core.flags |> shifter2 value.value

                        Shifter3 ->
                            z80_core.flags |> shifter3 value.value

                        Shifter4 ->
                            z80_core.flags |> shifter4 value.value

                        Shifter5 ->
                            z80_core.flags |> shifter5 value.value

                        Shifter6 ->
                            z80_core.flags |> shifter6 value.value

                        Shifter7 ->
                            z80_core.flags |> shifter7 value.value

                ( env_2, newTime2 ) =
                    z80_core.env |> setMem address result.value value.time

                newFlags =
                    result.flags
            in
            { z80_core | flags = { newFlags | a = result.value }, env = env_2 } |> CoreOnly

        SetBitIndirectA bitTest raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    env |> mem addr clockTime rom48k

                value =
                    input.value |> setBit bitTest

                flags =
                    z80_core.flags

                ( env_2, newTime2 ) =
                    env |> setMem addr value input.time
            in
            { z80_core | flags = { flags | a = value }, env = env_2 } |> CoreOnly

        ResetBitIndirectA bitTest raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                input =
                    env |> mem addr clockTime rom48k

                value =
                    input.value |> clearBit bitTest

                flags =
                    z80_core.flags

                ( env_2, newTime2 ) =
                    env |> setMem addr value input.time
            in
            { z80_core | flags = { flags | a = value }, env = env_2 } |> CoreOnly


applyShifter : Shifter -> Int -> CpuTimeCTime -> Z80ROM -> Z80Core -> Z80Core
applyShifter shifterFunc addr cpu_time rom48k z80 =
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

        ( env_2, newTime2 ) =
            z80.env |> setMem addr result.value value.time
    in
    { z80 | flags = result.flags, env = env_2 }


applyTripleChangeDelta : Z80ROM -> CpuTimeCTime -> TripleByteChange -> Z80Core -> CoreChange
applyTripleChangeDelta rom48k cpu_time z80changeData z80 =
    let
        env =
            z80.env
    in
    case z80changeData of
        NewSPRegister int ->
            { z80
                | env = { env | sp = int }
            }
                |> CoreOnly

        NewIXRegister int ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | ix = int }
            }
                |> CoreOnly

        NewIYRegister int ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | iy = int }
            }
                |> CoreOnly

        NewHLIndirect int ->
            let
                main =
                    z80.main

                value =
                    env |> mem16 int rom48k cpu_time
            in
            { z80
                | main = { main | hl = value.value16 }
            }
                |> CoreOnly

        NewIXIndirect int ->
            let
                main =
                    z80.main

                value =
                    env |> mem16 int rom48k cpu_time
            in
            { z80
                | main = { main | ix = value.value16 }
            }
                |> CoreOnly

        NewIYIndirect int ->
            let
                main =
                    z80.main

                value =
                    env |> mem16 int rom48k cpu_time
            in
            { z80
                | main = { main | iy = value.value16 }
            }
                |> CoreOnly

        NewAIndirect int ->
            let
                value =
                    env |> mem int cpu_time rom48k

                flags =
                    z80.flags
            in
            { z80
                | flags = { flags | a = value.value }
            }
                |> CoreOnly

        TripleSetIndirectFromA addr ->
            let
                ( env2, time2 ) =
                    env |> setMem addr z80.flags.a cpu_time
            in
            { z80 | env = env2 } |> CoreOnly

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
            }
                |> CoreOnly

        Store16BitFromHL address ->
            let
                value =
                    z80.main.hl

                ( env1, clockTime ) =
                    env |> setMem16 address value cpu_time
            in
            { z80
                | env = env1
            }
                |> CoreOnly


applyTripleFlagChange : TripleWithFlagsChange -> Z80Core -> CoreChange
applyTripleFlagChange z80changeData z80 =
    case z80changeData of
        Conditional16BitJump int function ->
            if z80.flags |> function then
                JumpOnlyPC int

            else
                z80 |> CoreOnly

        Conditional16BitCall address shortdelay function ->
            if z80.flags |> function then
                CallWithPCAndDelay address shortdelay

            else
                z80 |> CoreOnly

        CallImmediate int ->
            CallWithPC int

        NewPCRegister int ->
            JumpOnlyPC int


applyEdFourByte : CpuTimeCTime -> EDFourByteChange -> Z80ROM -> Z80Core -> CoreChange
applyEdFourByte clockTime z80changeData rom48k z80_core =
    case z80changeData of
        SetMemFrom int sixteenBit ->
            let
                reg =
                    case sixteenBit of
                        RegHL ->
                            z80_core.main.hl

                        RegDE ->
                            z80_core.main |> get_de

                        RegBC ->
                            z80_core.main |> get_bc

                        RegSP ->
                            z80_core.env.sp

                ( env, newTime ) =
                    z80_core.env |> setMem16 int reg clockTime
            in
            { z80_core | env = env } |> CoreOnly

        GetFromMem int sixteenBit ->
            let
                value =
                    z80_core.env |> mem16 int rom48k clockTime

                core =
                    case sixteenBit of
                        RegHL ->
                            let
                                z80_main =
                                    z80_core.main

                                main =
                                    { z80_main | hl = value.value16 }
                            in
                            { z80_core | main = main }

                        RegDE ->
                            let
                                main =
                                    z80_core.main |> set_de_main value.value16
                            in
                            { z80_core | main = main }

                        RegBC ->
                            let
                                main =
                                    z80_core.main |> set_bc_main value.value16
                            in
                            { z80_core | main = main }

                        RegSP ->
                            let
                                env =
                                    z80_core.env
                            in
                            { z80_core | env = { env | sp = value.value16 } }
            in
            core |> CoreOnly


applyEdRegisterDelta : CpuTimeCTime -> EDRegisterChange -> Z80ROM -> Z80Core -> CoreChange
applyEdRegisterDelta clockTime z80changeData rom48k z80_core =
    let
        env =
            z80_core.env
    in
    case z80changeData of
        EDNoOp ->
            z80_core |> CoreOnly

        RRD ->
            z80_core |> rrd rom48k clockTime |> CoreOnly

        RLD ->
            z80_core |> rld rom48k clockTime |> CoreOnly

        IN_C ->
            z80_core |> execute_ED70 rom48k clockTime |> CoreOnly

        IN_A_C ->
            z80_core |> execute_ED78 rom48k clockTime |> CoreOnly

        AdcHLSP ->
            z80_core |> adc_hl_sp rom48k clockTime |> CoreOnly

        Cpir direction repeat ->
            let
                ( core2, newTime, newpc ) =
                    z80_core |> cpir direction repeat rom48k clockTime
            in
            core2 |> Looper newpc

        SbcHL reg16type ->
            let
                reg =
                    case reg16type of
                        RegHL ->
                            z80_core.main.hl

                        RegDE ->
                            z80_core.main |> get_de

                        RegBC ->
                            z80_core.main |> get_bc

                        RegSP ->
                            z80_core.env.sp

                ( flags, main ) =
                    z80_core |> sbc_hl reg
            in
            { z80_core | main = main, flags = flags } |> CoreOnly

        Ldir direction repeat ->
            let
                ( result, maybeDelay, new_pc ) =
                    z80_core |> ldir direction repeat rom48k clockTime
            in
            case maybeDelay of
                Just a ->
                    result |> LooperWithDelay new_pc a

                Nothing ->
                    result |> Looper new_pc

        RegChangeIm intMode ->
            let
                interrupts =
                    debugLog "SetInterruptMode" intMode z80_core.interrupts
            in
            { z80_core
                | interrupts = { interrupts | iM = intMode }
            }
                |> CoreOnly

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
                    z80_core.env |> z80_in (main |> get_bc) rom48k.keyboard clockTime

                ( env_2, newTme2 ) =
                    env |> setMem main.hl in_value.value in_value.time

                pc2 =
                    if repeat && new_b /= 0 then
                        JumpBack

                    else
                        NoOffset

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
            { z80_core | env = env_2, flags = flags, main = new_main } |> Looper pc2

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

                outvalue =
                    z80_core.env |> mem main.hl clockTime rom48k

                main_2 =
                    { main | b = new_b, hl = new_hl }

                new_bc =
                    main_2 |> get_bc

                ( env2, newTime2 ) =
                    z80_core.env |> z80_out new_bc outvalue.value outvalue.time

                pc2 =
                    if repeat && new_b /= 0 then
                        JumpBack

                    else
                        NoOffset

                flags =
                    z80_core.flags |> inirOtirFlags new_hl new_bc outvalue.value
            in
            { z80_core | env = env2, flags = flags, main = main_2 } |> Looper pc2

        InRC changeMainRegister ->
            let
                z80_main =
                    z80_core.main

                z80_flags =
                    z80_core.flags

                in_value =
                    z80_core.env |> z80_in (z80_main |> get_bc) rom48k.keyboard clockTime

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
            { z80_core | flags = z80_flags |> f_szh0n0p in_value.value, main = main } |> CoreOnly
