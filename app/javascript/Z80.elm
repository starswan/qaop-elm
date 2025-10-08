--
-- $Id$
--


module Z80 exposing (..)

import Array exposing (Array)
import Bitwise
import CpuTimeCTime exposing (CTime(..), CpuTimeAndPc, CpuTimeAndValue, CpuTimeCTime, InstructionDuration(..), addCpuTimeTime, addDuration, c_TIME_LIMIT, reset_cpu_time)
import Dict exposing (Dict)
import DoubleWithRegisters exposing (doubleWithRegisters, doubleWithRegistersIX, doubleWithRegistersIY)
import GroupCB exposing (singleByteMainAndFlagRegistersCB, singleByteMainAndFlagRegistersIXCB, singleByteMainAndFlagRegistersIYCB, singleByteMainRegsCB, singleEnvMainRegsCB)
import GroupCBIXIY exposing (singleByteMainRegsIXCB, singleByteMainRegsIYCB, singleEnvMainRegsIXCB, singleEnvMainRegsIYCB)
import GroupED exposing (delta_dict_lite_E0, edWithInterrupts, singleByteFlagsED, singleByteMainAndFlagsED, singleByteMainRegsED)
import Loop
import PCIncrement exposing (InterruptPCIncrement(..), MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import Set
import SimpleFlagOps exposing (singleByteFlags, singleByteFlagsCB, singleByteFlagsDD, singleByteFlagsFD)
import SimpleSingleByte exposing (singleByteMainRegs, singleByteMainRegsDD, singleByteMainRegsFD)
import SingleByteWithEnv exposing (singleByteZ80Env)
import SingleEnvWithMain exposing (singleEnvMainRegs, singleEnvMainRegsIX, singleEnvMainRegsIY)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters, singleByteMainAndFlagRegistersIX, singleByteMainAndFlagRegistersIY)
import SingleNoParams exposing (ex_af, execute_0x76_halt, exx, singleWithNoParam, singleWithNoParamDD, singleWithNoParamFD)
import SingleWith8BitParameter exposing (maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam, tripleByteWith16BitParamDD, tripleByteWith16BitParamFD)
import TripleWithFlags exposing (triple16WithFlags)
import TripleWithMain exposing (tripleMainRegs, tripleMainRegsIX, tripleMainRegsIY)
import Z80Core exposing (Z80, Z80Core, di_0xF3, ei_0xFB)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..))
import Z80Env exposing (Z80Env, mem, mem16, z80env_constructor)
import Z80Execute exposing (DeltaWithChanges(..), apply_delta)
import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80OpCode exposing (fetchInstruction)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IntWithFlagsTimeAndPC, InterruptMode(..), InterruptRegisters, MainRegisters, MainWithIndexRegisters)


constructor : Z80
constructor =
    let
        main =
            MainWithIndexRegisters 0 0 0 0 0 0 0

        alternate =
            MainRegisters 0 0 0 0 0

        flags =
            FlagRegisters 0 0 0 0 0

        --alt_flags =
        --    FlagRegisters 0 0 0 0 0
        interrupts =
            InterruptRegisters IM0 False 0 0 0

        time =
            reset_cpu_time
    in
    --Z80 z80env_constructor 0 main main_flags alternate alt_flags 0 interrupts
    Z80 (Z80Core z80env_constructor 0 main flags interrupts time) alternate flags



--	int a() {return A;}
--	int f() {return flags();}
--get_f: Z80 -> Int
--get_f z80 =
--    get_flags z80
--get_i: Z80 -> Int
--get_i z80 =
--    shiftRightBy8 z80.interrupts.ir
--	int r() {return R&0x7F | IR&0x80;}
--get_r: Z80 -> Int
--get_r z80 =
--    let
--        a = and z80.interrupts.r 0x7F
--        b = and z80.interrupts.ir 0x80
--    in
--        or a b
--	int im() {int v=IM; return v==0?v:v-1;}
--get_im: Z80 -> Int
--get_im z80 =
--    let
--        v = z80.interrupts.iM
--    in
--        if v == 0 then v else v - 1
--	void f(int v) {flags(v);}
--set_f: Int -> Int -> FlagRegisters
--set_f v a =
--   set_flags v a
--	void hl(int v) {HL = v;}
--set_hl: Int -> Z80 -> Z80
--set_hl hl z80 =
--    let
--        z80_main = z80.main
--    in
--        { z80 | main = { z80_main | hl = hl } }
--	void ix(int v) {IX = v;}
--set_ix: Z80 -> Int -> Z80
--set_ix z80 ix =
--    { z80 | ix = ix }
--	void iy(int v) {IY = v;}
--set_iy: Z80 -> Int -> Z80
--set_iy z80 iy =
--    { z80 | iy = iy }
--	void sp(int v) {SP = v;}
--set_sp: Int -> Z80 -> Z80
--set_sp sp z80 =
--   { z80 | sp = Bitwise.and sp 0xFFFF }
--	void r(int v) {R=v; IR = IR&0xFF00 | v&0x80;}
--	void im(int v) {IM = v+1 & 3;}
--	void ei(boolean v) {IFF = v ? 3 : 0;}
--
--
--
--
--	private void rrd()
--	{
--		int v = env.mem(HL) | A<<8;
--		time += 7;
--		f_szh0n0p(A = A&0xF0 | v&0x0F);
--		env.mem(HL, v>>>4 & 0xFF);
--		MP = HL+1;
--		time += 3;
--	}
--
--
--	private void ld_a_ir(int v)
--	{
--		Ff = Ff&~0xFF | (A = v);
--		Fr = v==0 ? 0 : 1;
--		Fa = Fb = IFF<<6 & 0x80;
--		time++;
--	}
--
--
--
--	/* Note: EI isn't prefix here - interrupt will be acknowledged */
--


execute_ltC0 : Int -> Z80ROM -> Z80Core -> Maybe Z80Delta
execute_ltC0 c rom48k z80 =
    case lt40_array_lite |> Array.get c |> Maybe.withDefault Nothing of
        Just f_without_ixiyhl ->
            Just (z80 |> f_without_ixiyhl rom48k)

        Nothing ->
            Nothing


list0255 =
    List.range 0 255


lt40_array_lite : Array (Maybe (Z80ROM -> Z80Core -> Z80Delta))
lt40_array_lite =
    let
        delta_funcs =
            list0255 |> List.map (\index -> delta_dict_lite_E0 |> Dict.get index)
    in
    delta_funcs |> Array.fromList



-- case 0xC7:
-- case 0xCF:
-- case 0xD7:
-- case 0xDF:
-- case 0xE7:
-- case 0xEF:
-- case 0xF7:
-- case 0xFF: push(PC); PC=c-199; break;
--execute_gtc0: Int -> IXIYHL -> Z80 -> Z80Delta
--execute_gtc0 c ixiyhl z80 =
--   case c of
--      -- case 0xC7:
--      -- case 0xCF:
--      -- case 0xD7:
--      -- case 0xDF:
--      -- case 0xE7:
--      -- case 0xEF:
--      -- case 0xF7:
--      -- case 0xFF: push(PC); PC=c-199; break;
--      0xC7 -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xC7 |> Whole
--      0xCF -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xCF |> Whole
--      0xD7 -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xD7 |> Whole
--      0xDF -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xDF |> Whole
--      0xE7 -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xE7 |> Whole
--      0xEF -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xEF |> Whole
--      0xF7 -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xF7 |> Whole
--      0xFF -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xFF |> Whole
--      ---- case 0xDC: call((Ff&0x100)!=0); break;
--      --0xDC -> z80 |> call (Bitwise.and z80.flags.ff 0x100 /= 0)
--      ---- case 0xF2: jp((Ff&FS)==0); break;
--      --0xF2 -> z80 |> jp (Bitwise.and z80.flags.ff c_FS == 0)
--      ---- case 0xFA: jp((Ff&FS)!=0); break;
--      --0xFA -> z80 |> jp (Bitwise.and z80.flags.ff c_FS /= 0)
--      -- case 0xDA: jp((Ff&0x100)!=0); break;
--      --0xDA -> z80 |> jp ((Bitwise.and z80.flags.ff 0x100) /= 0)
--      -- case 0xCE: adc(imm8()); break;
--      --0xCE -> let
--      --           v = z80 |> imm8
--      --           flags = z80.flags |> adc v.value
--      --        in
--      --           {z80 | pc = v.pc, env = v.env, flags = flags }
--      _ -> debug_todo "execute" (c |> toHexString) z80  |> Whole
-- from z80_transform data:
--[ parseSimpleSingleByte singleByteMainRegs OneByteInstruction
--, parseSingleByteWithFlags plainSingleByteFlags
--, parseTriple16Param standardTriple16Bit 1
--, parseRelativeJump
--, parseSingleEnvMain standardSingleEnvMain OneByteInstruction
--, parseTriple16Flags 1
--, parseSingleByteMainAndFlags singleByteMainAndFlagRegisters OneByteInstruction
--, parseDoubleWithRegs standardDouble
--, parseTripleMain standardTripleMain 1
--, parseSingleEnv
--, parseSingleByteWithParam
--]


type ExecutionType
    = Ordinary Int CpuTimeCTime
    | IndexIX CpuTimeAndValue
    | IndexIY CpuTimeAndValue
    | Special SpecialExecutionType


type SpecialExecutionType
    = BitManipCB CpuTimeAndValue
    | IXCB Int CpuTimeAndValue
    | IYCB Int CpuTimeAndValue
    | EDMisc CpuTimeAndValue


executeAndApplyDelta : CpuTimeAndValue -> Z80ROM -> Z80Core -> Z80Core
executeAndApplyDelta ct rom48k z80 =
    z80 |> execute_delta ct rom48k |> apply_delta z80 rom48k


execute_delta : CpuTimeAndValue -> Z80ROM -> Z80Core -> DeltaWithChanges
execute_delta ct rom48k z80 =
    --int v, c = env.m1(PC, IR|R++&0x7F);
    --PC = (char)(PC+1); time += 4;
    --switch(c) {
    let
        executionType =
            case ct.value of
                0xCB ->
                    let
                        param =
                            z80.env |> mem (Bitwise.and (z80.pc + 1) 0xFFFF) ct.time rom48k
                    in
                    Special (BitManipCB param)

                0xED ->
                    let
                        param =
                            z80.env |> mem (Bitwise.and (z80.pc + 1) 0xFFFF) ct.time rom48k
                    in
                    Special (EDMisc param)

                0xDD ->
                    let
                        param =
                            z80.env |> mem (Bitwise.and (z80.pc + 1) 0xFFFF) ct.time rom48k
                    in
                    if param.value == 0xCB then
                        let
                            ixcboffset =
                                z80.env |> mem (Bitwise.and (z80.pc + 2) 0xFFFF) ct.time rom48k

                            ixcbparam =
                                z80.env |> mem (Bitwise.and (z80.pc + 3) 0xFFFF) ct.time rom48k
                        in
                        Special (IXCB ixcboffset.value ixcbparam)

                    else
                        IndexIX param

                0xFD ->
                    let
                        param =
                            z80.env |> mem (Bitwise.and (z80.pc + 1) 0xFFFF) ct.time rom48k
                    in
                    if param.value == 0xCB then
                        let
                            iycboffset =
                                z80.env |> mem (Bitwise.and (z80.pc + 2) 0xFFFF) ct.time rom48k

                            iycbparam =
                                z80.env |> mem (Bitwise.and (z80.pc + 3) 0xFFFF) ct.time rom48k
                        in
                        Special (IYCB iycboffset.value iycbparam)

                    else
                        IndexIY param

                _ ->
                    Ordinary ct.value ct.time
    in
    case executionType of
        Ordinary int cpuTimeCTime ->
            runOrdinary int cpuTimeCTime rom48k z80

        IndexIX cpuTimeAndValue ->
            runIndexIX cpuTimeAndValue rom48k z80

        IndexIY cpuTimeAndValue ->
            runIndexIY cpuTimeAndValue rom48k z80

        Special specialExecutionType ->
            runSpecial specialExecutionType rom48k z80


runOrdinary : Int -> CpuTimeCTime -> Z80ROM -> Z80Core -> DeltaWithChanges
runOrdinary ct_value instrTime rom48k z80_core =
    case singleByteMainRegs |> Dict.get ct_value of
        Just ( mainRegFunc, duration ) ->
            RegisterChangeDelta IncrementByOne duration (mainRegFunc z80_core.main)

        Nothing ->
            case singleByteFlags |> Dict.get ct_value of
                Just ( flagFunc, duration ) ->
                    FlagDelta IncrementByOne duration (flagFunc z80_core.flags)

                Nothing ->
                    let
                        triple16 =
                            tripleByteWith16BitParam |> Dict.get ct_value
                    in
                    case triple16 of
                        Just ( f, duration ) ->
                            let
                                env =
                                    z80_core.env

                                newTime =
                                    instrTime |> addDuration duration

                                doubleParam =
                                    env |> mem16 (Bitwise.and (z80_core.pc + 1) 0xFFFF) rom48k newTime
                            in
                            Triple16ParamDelta doubleParam.time TripleIncrementByThree (f doubleParam.value16)

                        Nothing ->
                            case maybeRelativeJump |> Dict.get ct_value of
                                Just ( f, duration ) ->
                                    let
                                        newTime =
                                            instrTime |> addDuration duration

                                        param =
                                            z80_core.env |> mem (Bitwise.and (z80_core.pc + 1) 0xFFFF) newTime rom48k
                                    in
                                    JumpChangeDelta param.time (f param.value z80_core.flags)

                                Nothing ->
                                    case singleEnvMainRegs |> Dict.get ct_value of
                                        Just ( f, duration ) ->
                                            MainWithEnvDelta IncrementByOne duration (f z80_core.main rom48k instrTime z80_core.env)

                                        Nothing ->
                                            case triple16WithFlags |> Dict.get ct_value of
                                                Just ( f, duration ) ->
                                                    let
                                                        env =
                                                            z80_core.env

                                                        env_1 =
                                                            instrTime |> addDuration duration

                                                        doubleParam =
                                                            env |> mem16 (Bitwise.and (z80_core.pc + 1) 0xFFFF) rom48k env_1
                                                    in
                                                    Triple16FlagsDelta doubleParam.time (f doubleParam.value16 z80_core.flags)

                                                Nothing ->
                                                    case singleByteMainAndFlagRegisters |> Dict.get ct_value of
                                                        Just ( f, duration ) ->
                                                            PureDelta IncrementByOne (instrTime |> addDuration duration) (f z80_core.main z80_core.flags)

                                                        Nothing ->
                                                            case doubleWithRegisters |> Dict.get ct_value of
                                                                Just ( f, duration ) ->
                                                                    let
                                                                        time =
                                                                            instrTime |> addDuration duration

                                                                        param =
                                                                            --case pcInc of
                                                                            --    IncreaseByTwo ->
                                                                            z80_core.env |> mem (Bitwise.and (z80_core.pc + 1) 0xFFFF) time rom48k

                                                                        --IncreaseByThree ->
                                                                        --    z80_core.env |> mem (Bitwise.and (z80_core.pc + 2) 0xFFFF) time rom48k
                                                                    in
                                                                    DoubleWithRegistersDelta IncreaseByTwo param.time (f z80_core.main param.value)

                                                                Nothing ->
                                                                    case tripleMainRegs |> Dict.get ct_value of
                                                                        Just ( f, duration ) ->
                                                                            let
                                                                                time =
                                                                                    instrTime |> addDuration duration

                                                                                env =
                                                                                    z80_core.env

                                                                                doubleParam =
                                                                                    env |> mem16 (Bitwise.and (z80_core.pc + 1) 0xFFFF) rom48k time
                                                                            in
                                                                            TripleMainChangeDelta doubleParam.time TripleIncrementByThree (f doubleParam.value16 z80_core.main)

                                                                        Nothing ->
                                                                            case singleByteZ80Env |> Dict.get ct_value of
                                                                                Just ( f, duration ) ->
                                                                                    SingleEnvDelta (instrTime |> addDuration duration) (f z80_core.env)

                                                                                Nothing ->
                                                                                    case singleWithNoParam |> Dict.get ct_value of
                                                                                        Just ( f, duration ) ->
                                                                                            NoParamsDelta (instrTime |> addDuration duration) f

                                                                                        Nothing ->
                                                                                            case singleByte instrTime ct_value z80_core rom48k of
                                                                                                Just deltaThing ->
                                                                                                    deltaThing

                                                                                                Nothing ->
                                                                                                    oldDelta ct_value instrTime z80_core.interrupts z80_core rom48k


runIndexIX : CpuTimeAndValue -> Z80ROM -> Z80Core -> DeltaWithChanges
runIndexIX param rom48k z80 =
    case singleByteMainRegsDD |> Dict.get param.value of
        Just ( mainRegFunc, duration ) ->
            RegisterChangeDelta IncrementByTwo duration (mainRegFunc z80.main)

        Nothing ->
            case singleByteFlagsDD |> Dict.get param.value of
                Just ( flagFunc, duration ) ->
                    FlagDelta IncrementByTwo duration (flagFunc z80.flags)

                Nothing ->
                    let
                        ( triple16, instrTime, paramOffset ) =
                            ( tripleByteWith16BitParamDD |> Dict.get param.value, param.time, 2 )
                    in
                    case triple16 of
                        Just ( f, duration ) ->
                            let
                                env =
                                    z80.env

                                env_1 =
                                    instrTime |> addDuration duration

                                doubleParam =
                                    env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k env_1
                            in
                            Triple16ParamDelta doubleParam.time TripleIncrementByFour (f doubleParam.value16)

                        Nothing ->
                            case singleEnvMainRegsIX |> Dict.get param.value of
                                Just ( f, duration ) ->
                                    MainWithEnvDelta IncrementByTwo duration (f z80.main rom48k z80.env)

                                Nothing ->
                                    case singleByteMainAndFlagRegistersIX |> Dict.get param.value of
                                        Just ( f, duration ) ->
                                            PureDelta IncrementByTwo (instrTime |> addDuration duration) (f z80.main z80.flags)

                                        Nothing ->
                                            case doubleWithRegistersIX |> Dict.get param.value of
                                                Just ( f, duration ) ->
                                                    let
                                                        time =
                                                            instrTime |> addDuration duration

                                                        doubleParam =
                                                            z80.env |> mem (Bitwise.and (z80.pc + 2) 0xFFFF) time rom48k
                                                    in
                                                    DoubleWithRegistersDelta IncreaseByThree doubleParam.time (f z80.main doubleParam.value)

                                                Nothing ->
                                                    case tripleMainRegsIX |> Dict.get param.value of
                                                        Just ( f, pcInc, duration ) ->
                                                            let
                                                                time =
                                                                    instrTime |> addDuration duration

                                                                env =
                                                                    z80.env

                                                                doubleParam =
                                                                    env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k time
                                                            in
                                                            TripleMainChangeDelta doubleParam.time pcInc (f doubleParam.value16 z80.main)

                                                        Nothing ->
                                                            case singleWithNoParamDD |> Dict.get param.value of
                                                                Just ( f, duration ) ->
                                                                    NoParamsDelta (instrTime |> addDuration duration) f

                                                                Nothing ->
                                                                    --oldDelta 0xDD instrTime z80.interrupts z80 rom48k
                                                                    UnknownInstruction "execute IndexIX" param.value


runIndexIY : CpuTimeAndValue -> Z80ROM -> Z80Core -> DeltaWithChanges
runIndexIY param rom48k z80 =
    case singleByteMainRegsFD |> Dict.get param.value of
        Just ( mainRegFunc, duration ) ->
            RegisterChangeDelta IncrementByTwo duration (mainRegFunc z80.main)

        Nothing ->
            case singleByteFlagsFD |> Dict.get param.value of
                Just ( flagFunc, duration ) ->
                    FlagDelta IncrementByTwo duration (flagFunc z80.flags)

                Nothing ->
                    let
                        ( triple16, instrTime, paramOffset ) =
                            ( tripleByteWith16BitParamFD |> Dict.get param.value, param.time, 2 )
                    in
                    case triple16 of
                        Just ( f, duration ) ->
                            let
                                env =
                                    z80.env

                                env_1 =
                                    instrTime |> addDuration duration

                                doubleParam =
                                    env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k env_1
                            in
                            Triple16ParamDelta doubleParam.time TripleIncrementByFour (f doubleParam.value16)

                        Nothing ->
                            case singleEnvMainRegsIY |> Dict.get param.value of
                                Just ( f, duration ) ->
                                    MainWithEnvDelta IncrementByTwo duration (f z80.main rom48k z80.env)

                                Nothing ->
                                    case singleByteMainAndFlagRegistersIY |> Dict.get param.value of
                                        Just ( f, duration ) ->
                                            PureDelta IncrementByTwo (instrTime |> addDuration duration) (f z80.main z80.flags)

                                        Nothing ->
                                            case doubleWithRegistersIY |> Dict.get param.value of
                                                Just ( f, duration ) ->
                                                    let
                                                        time =
                                                            instrTime |> addDuration duration

                                                        doubleParam =
                                                            z80.env |> mem (Bitwise.and (z80.pc + 2) 0xFFFF) time rom48k
                                                    in
                                                    DoubleWithRegistersDelta IncreaseByThree doubleParam.time (f z80.main doubleParam.value)

                                                Nothing ->
                                                    case tripleMainRegsIY |> Dict.get param.value of
                                                        Just ( f, pcInc, duration ) ->
                                                            let
                                                                time =
                                                                    instrTime |> addDuration duration

                                                                env =
                                                                    z80.env

                                                                doubleParam =
                                                                    env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k time
                                                            in
                                                            TripleMainChangeDelta doubleParam.time pcInc (f doubleParam.value16 z80.main)

                                                        Nothing ->
                                                            case singleWithNoParamFD |> Dict.get param.value of
                                                                Just ( f, duration ) ->
                                                                    NoParamsDelta (instrTime |> addDuration duration) f

                                                                Nothing ->
                                                                    --oldDelta 0xFD instrTime z80.interrupts z80 rom48k
                                                                    UnknownInstruction "execute IndexIY" param.value


runSpecial : SpecialExecutionType -> Z80ROM -> Z80Core -> DeltaWithChanges
runSpecial specialType rom48k z80_core =
    case specialType of
        BitManipCB param ->
            let
                instrTime =
                    param.time
            in
            case singleByteMainRegsCB |> Dict.get param.value of
                Just ( mainRegFunc, duration ) ->
                    RegisterChangeDelta IncrementByTwo duration (mainRegFunc z80_core.main)

                Nothing ->
                    case singleByteFlagsCB |> Dict.get param.value of
                        Just ( flagFunc, duration ) ->
                            FlagDelta IncrementByTwo duration (flagFunc z80_core.flags)

                        Nothing ->
                            case singleEnvMainRegsCB |> Dict.get param.value of
                                Just ( f, duration ) ->
                                    MainWithEnvDelta IncrementByTwo duration (f z80_core.main rom48k z80_core.env)

                                Nothing ->
                                    case singleByteMainAndFlagRegistersCB |> Dict.get param.value of
                                        Just ( f, duration ) ->
                                            PureDelta IncrementByTwo (instrTime |> addDuration duration) (f z80_core.main z80_core.flags)

                                        Nothing ->
                                            UnknownInstruction "execute CB" param.value

        IXCB offset param ->
            let
                instrTime =
                    param.time
            in
            case singleByteMainRegsIXCB |> Dict.get param.value |> Maybe.map (\( f, d ) -> ( f offset, d )) of
                Just ( mainRegFunc, duration ) ->
                    RegisterChangeDelta IncrementByFour duration (mainRegFunc z80_core.main)

                Nothing ->
                    case singleByteFlagsCB |> Dict.get param.value of
                        Just ( flagFunc, duration ) ->
                            FlagDelta IncrementByFour duration (flagFunc z80_core.flags)

                        Nothing ->
                            case singleEnvMainRegsIXCB |> Dict.get param.value of
                                Just ( f, duration ) ->
                                    MainWithEnvDelta IncrementByFour duration (f z80_core.main offset rom48k z80_core.env)

                                Nothing ->
                                    case singleByteMainAndFlagRegistersIXCB |> Dict.get param.value of
                                        Just ( f, duration ) ->
                                            PureDelta IncrementByFour (instrTime |> addDuration duration) (f z80_core.main z80_core.flags)

                                        Nothing ->
                                            -- This fails on DD CB 3D xx
                                            UnknownInstruction "execute IXCB" param.value

        --oldDelta 0xDD instrTime z80.interrupts z80 rom48k
        --UnknownInstruction "execute IXCB" param.value
        IYCB iycboffset param ->
            let
                instrTime =
                    param.time
            in
            case singleByteMainRegsIYCB |> Dict.get param.value |> Maybe.map (\( f, d ) -> ( f iycboffset, d )) of
                Just ( mainRegFunc, duration ) ->
                    RegisterChangeDelta IncrementByFour duration (mainRegFunc z80_core.main)

                Nothing ->
                    case singleByteFlagsCB |> Dict.get param.value of
                        Just ( flagFunc, duration ) ->
                            FlagDelta IncrementByFour duration (flagFunc z80_core.flags)

                        Nothing ->
                            case singleEnvMainRegsIYCB |> Dict.get param.value of
                                Just ( f, duration ) ->
                                    MainWithEnvDelta IncrementByFour duration (f z80_core.main iycboffset rom48k z80_core.env)

                                Nothing ->
                                    case singleByteMainAndFlagRegistersIYCB |> Dict.get param.value of
                                        Just ( f, duration ) ->
                                            PureDelta IncrementByFour (instrTime |> addDuration duration) (f z80_core.main z80_core.flags)

                                        Nothing ->
                                            -- This fails on FD CB 3D xx
                                            UnknownInstruction "execute IYCB" param.value

        --oldDelta 0xFD instrTime z80.interrupts z80 rom48k
        EDMisc param ->
            let
                instrTime =
                    param.time
            in
            case singleByteMainRegsED |> Dict.get param.value of
                Just ( mainRegFunc, duration ) ->
                    EDChangeDelta IncrementByTwo duration (mainRegFunc z80_core.main)

                Nothing ->
                    case singleByteFlagsED |> Dict.get param.value of
                        Just ( flagFunc, duration ) ->
                            FlagDelta IncrementByTwo duration (flagFunc z80_core.flags)

                        Nothing ->
                            case singleByteMainAndFlagsED |> Dict.get param.value of
                                Just ( f, pcInc, duration ) ->
                                    PureDelta pcInc (instrTime |> addDuration duration) (f z80_core.main z80_core.flags)

                                Nothing ->
                                    case edWithInterrupts |> Dict.get param.value of
                                        Just ( f, duration ) ->
                                            InterruptDelta AddTwoToPC duration (f z80_core.interrupts)

                                        Nothing ->
                                            oldDelta 0xED instrTime z80_core.interrupts z80_core rom48k



--UnknownInstruction "execute ED" param.value
--UnknownInstruction "execute IYCB" param.value
-- case 0xD4: call((Ff&0x100)==0); break;
-- case 0xE4: call((flags()&FP)==0); break;
-- case 0xEC: call((flags()&FP)!=0); break;
-- case 0xF4: call((Ff&FS)==0); break;
-- case 0xFC: call((Ff&FS)!=0); break;
-- case 0xF3: IFF=0; break;


singleByte : CpuTimeCTime -> Int -> Z80Core -> Z80ROM -> Maybe DeltaWithChanges
singleByte ctime instrCode tmp_z80 rom48k =
    case singleWith8BitParam |> Dict.get instrCode of
        Just ( f, duration ) ->
            let
                instrTime =
                    ctime |> addDuration duration

                param =
                    --case pcInc of
                    --    IncreaseByTwo ->
                    tmp_z80.env |> mem (Bitwise.and (tmp_z80.pc + 1) 0xFFFF) instrTime rom48k

                --IncreaseByThree ->
                --    tmp_z80.env |> mem (Bitwise.and (tmp_z80.pc + 2) 0xFFFF) instrTime rom48k
            in
            Just (Simple8BitDelta IncreaseByTwo param.time (f param.value))

        Nothing ->
            Nothing


oldDelta : Int -> CpuTimeCTime -> InterruptRegisters -> Z80Core -> Z80ROM -> DeltaWithChanges
oldDelta c_value c_time interrupts tmp_z80 rom48k =
    let
        new_pc =
            Bitwise.and (tmp_z80.pc + 1) 0xFFFF

        z80 =
            { tmp_z80 | pc = new_pc }

        new_time =
            c_time |> addCpuTimeTime 4
    in
    --z80
    --   |> execute_ltC0 c.value rom48k
    --   |> Maybe.map (\z80delta ->  OldDeltaWithChanges (DeltaWithChangesData z80delta interrupts new_pc new_time))
    --   |> withDefaultLazy (\() ->
    --       let
    --            delta = debugTodo "execute" (c.value |> toHexString) z80  |> Whole
    --       in
    --       OldDeltaWithChanges (DeltaWithChangesData delta interrupts new_pc new_time)
    --       )
    case z80 |> execute_ltC0 c_value rom48k of
        Just z80delta ->
            OldDeltaWithChanges (DeltaWithChangesData z80delta interrupts new_pc new_time)

        Nothing ->
            --case c.value of
            --0xDD -> DeltaWithChanges (group_xy IXIY_IX z80) interrupts new_pc new_time
            --0xFD -> DeltaWithChanges (group_xy IXIY_IY z80) interrupts new_pc new_time
            --0xED -> DeltaWithChanges (Whole (group_ed z80)) interrupts new_pc new_time
            --0xCD -> DeltaWithChanges (execute_0xCD z80) interrupts new_pc new_time
            --_ ->
            let
                delta =
                    UnknownIntValue "execute" c_value

                --debugTodo "execute" (c.value |> toHexString) z80 |> Whole
            in
            OldDeltaWithChanges (DeltaWithChangesData delta interrupts new_pc new_time)


executeCoreInstruction : Z80ROM -> Z80Core -> Z80Core
executeCoreInstruction rom48k z80 =
    let
        ct =
            fetchInstruction rom48k 0 z80
    in
    z80 |> executeAndApplyDelta ct rom48k


c_EX_AF_AFDASH =
    0x08


c_EXX =
    0xD9


c_HALT =
    0x76


c_DI =
    0xF3


c_EI =
    0xFB


nonCoreFuncs : Dict Int ( Z80 -> Z80, InstructionDuration )
nonCoreFuncs =
    Dict.fromList
        [ ( c_EX_AF_AFDASH, ( ex_af, FourTStates ) )
        , ( c_EXX, ( exx, FourTStates ) )
        , ( c_HALT, ( execute_0x76_halt, FourTStates ) )
        , ( c_DI, ( di_0xF3, FourTStates ) )
        , ( c_EI, ( ei_0xFB, FourTStates ) )
        ]


nonCoreOpCodeList =
    nonCoreFuncs |> Dict.keys


nonCoreOpCodeSet =
    nonCoreOpCodeList |> Set.fromList


isCoreOpCode : Int -> Bool
isCoreOpCode value =
    --nonCoreOpCodeSet |> Set.member value |> not
    --value /= c_EX_AF_AFDASH && value /= c_EXX && value /= c_HALT && value /= c_DI && value /= c_EI
    -- This is still much faster than the set membership or the custom check
    nonCoreOpCodeList |> List.member value |> not


stillLooping : Z80Core -> Bool
stillLooping z80core =
    c_TIME_LIMIT > z80core.clockTime.cpu_time


coreLooping : ( Z80Core, CpuTimeAndValue, Int ) -> Bool
coreLooping ( z80core, timeAndValue, _ ) =
    isCoreOpCode timeAndValue.value && (z80core |> stillLooping)


executeCore : Z80ROM -> Z80 -> Z80
executeCore rom48k z80 =
    let
        z80_core =
            z80.core

        execute_f =
            \( core, ct, r_register ) ->
                let
                    core_1 =
                        core |> executeAndApplyDelta ct rom48k
                in
                ( core_1, fetchInstruction rom48k r_register core_1, r_register + 1 )

        ( core_2, ct1, new_r ) =
            Loop.while coreLooping execute_f ( z80_core, fetchInstruction rom48k z80_core.interrupts.r z80_core, z80_core.interrupts.r )

        core_ints =
            core_2.interrupts

        z80_1 =
            { z80 | core = { core_2 | interrupts = { core_ints | r = new_r } } }
    in
    case nonCoreFuncs |> Dict.get ct1.value of
        Just ( f, duration ) ->
            let
                z80_2 =
                    z80_1 |> f

                core =
                    z80_2.core

                ints =
                    core.interrupts

                newTime =
                    core.clockTime |> addDuration duration

                pc =
                    Bitwise.and (core.pc + 1) 0xFFFF
            in
            { z80_2 | core = { core | pc = pc, clockTime = newTime, interrupts = { ints | r = ints.r + 1 } } }

        Nothing ->
            z80_1


execute : Z80ROM -> Z80 -> Z80
execute rom48k z80 =
    let
        z80_core =
            z80.core
    in
    if z80_core.interrupts.halted then
        z80 |> execute_0x76_halt

    else
        let
            execute_f =
                executeCore rom48k
        in
        Loop.while (\x -> stillLooping x.core) execute_f z80



--	void execute()
--	{
--		if(halted) {
--			halt();
--			return;
--		}
--		do {
--			int v, c = env.m1(PC, IR|R++&0x7F);
--			PC = (char)(PC+1); time += 4;
--			switch(c) {
--// -------------- >8 main
--// case 0x00: break;
-- case 0x08: ex_af(); break;
-- case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
--	if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
--	PC=(char)v;} break;
--// -------------- >8
--			}
--		} while(time_limit - time > 0);
--	}
--
--	private void group_xy(int c0)
--	{
--		for(;;) {
--			int xy = c0==0xDD ? IX : IY;
--			int v, c = env.m1(PC, IR|R++&0x7F);
--			PC = (char)(PC+1); time += 4;
--			switch(c) {
--// -------------- >8 xy
--group_xy : IXIY -> Z80ROM -> Z80Core -> Z80Delta
--group_xy ixiy rom48k old_z80 =
--    let
--        c =
--            old_z80.env |> m1 old_z80.pc (or old_z80.interrupts.ir (and old_z80.r 0x7F)) rom48k
--
--        --intr = old_z80.interrupts
--        env =
--            old_z80.env
--
--        z80_1 =
--            { old_z80 | env = { env | time = c.time }, r = old_z80.r + 1 }
--
--        new_pc =
--            z80_1 |> inc_pc
--
--        z80 =
--            { z80_1 | pc = new_pc } |> add_cpu_time 4
--
--        ltc0 =
--            z80 |> execute_ltC0_xy c.value ixiy rom48k
--    in
--    case ltc0 of
--        Just z_z80 ->
--            z_z80
--
--        Nothing ->
--            --debugTodo "group_xy" (c.value |> toHexString) z80 |> Whole
--            UnknownIntValue "group_xy" c.value
--_ -> case ixiy of
--        IXIY_IX -> execute_gtc0 c.value IX z80
--        IXIY_IY -> execute_gtc0 c.value IY z80
--      case c.value of
-- case 0xED: group_ed(); break;
-- case 0xC0: time++; if(Fr!=0) MP=PC=pop(); break;
-- case 0xC2: jp(Fr!=0); break;
-- case 0xC8: time++; if(Fr==0) MP=PC=pop(); break;
-- case 0xCA: jp(Fr==0); break;
-- case 0xCC: call(Fr==0); break;
-- case 0xD0: time++; if((Ff&0x100)==0) MP=PC=pop(); break;
-- case 0xD2: jp((Ff&0x100)==0); break;
-- case 0xD8: time++; if((Ff&0x100)!=0) MP=PC=pop(); break;
-- case 0xDA: jp((Ff&0x100)!=0); break;
-- case 0xDC: call((Ff&0x100)!=0); break;
-- case 0xE0: time++; if((flags()&FP)==0) MP=PC=pop(); break;
-- case 0xE2: jp((flags()&FP)==0); break;
-- case 0xE4: call((flags()&FP)==0); break;
-- case 0xE8: time++; if((flags()&FP)!=0) MP=PC=pop(); break;
-- case 0xEA: jp((flags()&FP)!=0); break;
-- case 0xEC: call((flags()&FP)!=0); break;
-- case 0xF0: time++; if((Ff&FS)==0) MP=PC=pop(); break;
-- case 0xF2: jp((Ff&FS)==0); break;
-- case 0xF4: call((Ff&FS)==0); break;
-- case 0xF8: time++; if((Ff&FS)!=0) MP=PC=pop(); break;
-- case 0xFA: jp((Ff&FS)!=0); break;
-- case 0xFC: call((Ff&FS)!=0); break;
-- case 0xC1: bc(pop()); break;
-- case 0xC5: push(bc()); break;
-- case 0xD1: de(pop()); break;
-- case 0xD5: push(de()); break;
-- case 0xF1: af(pop()); break;
-- case 0xF5: push(A<<8|flags()); break;
-- case 0xC3: MP=PC=imm16(); break;
-- case 0xC6: add(imm8()); break;
-- case 0xCE: adc(imm8()); break;
-- case 0xD6: sub(imm8()); break;
-- case 0xDE: sbc(imm8()); break;
-- case 0xE6: and(imm8()); break;
-- case 0xEE: xor(imm8()); break;
-- case 0xF6: or(imm8()); break;
-- case 0xFE: cp(imm8()); break;
-- case 0xC9: MP=PC=pop(); break;
-- case 0xCD: call(true); break;
-- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
-- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
-- case 0xEB: v=HL; HL=de(); de(v); break;
-- case 0xF3: IFF=0; break;
-- case 0xFB: IFF=3; break;
-- case 0xF9: SP=xy; time+=2; break;
-- case 0xC7:
-- case 0xCF:
-- case 0xD7:
-- case 0xDF:
-- case 0xE7:
-- case 0xEF:
-- case 0xF7:
-- case 0xFF: push(PC); PC=c-199; break;
--         _ -> z80
--// -------------- >8
--			}
--			if(c0==0xDD) IX = xy; else IY = xy;
--			break;
--		}
--	}
--
--set_env: Z80Env -> Z80 -> Z80
--set_env z80env z80 =
--   { z80 | env = z80env }
--
--	void nmi()
--	{
--		IFF &= 2;
--		halted = false;
--		push(PC);
--		time += 4;
--		PC = 0x66;
--	}
--
--	void reset() {
--		halted = false;
--		PC = IFF = IM = 0;
--		af(SP = 0xFFFF);
--	}
--}
