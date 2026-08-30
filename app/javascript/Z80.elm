--
-- $Id$
--


module Z80 exposing (..)

import Bitwise
import CpuTimeCTime exposing (CTime(..), CpuTimeAndPc, CpuTimeAndValue, CpuTimeCTime, CpuTimePcAnd16BitValue, InstructionDuration(..), addDuration, addExtraCpuTime, c_TIME_LIMIT, reset_cpu_time)
import Dict exposing (Dict)
import GroupCB exposing (singleByteMainAndFlagRegistersCB, singleByteMainRegsCB, singleEnvMainRegsCB)
import GroupCBIXIY exposing (singleByteMainRegsIXCB, singleByteMainRegsIYCB, singleEnvMainRegsIXCB, singleEnvMainRegsIYCB)
import GroupED exposing (edWithInterrupts, fourByteMainED, singleByteFlagsED, singleByteMainAndFlagsED, singleByteMainRegsED)
import Interrupts exposing (IFFValue(..), InterruptMode(..))
import Loop
import OpcodeTables exposing (singleByteInstructions, singleByteMainFlagsRegsIX, singleByteMainFlagsRegsIY, threeByteInstructions, threeByteWithRegistersIX, threeByteWithRegistersIY, twoByteInstructions, twoByteWithRegistersIX, twoByteWithRegistersIY)
import PCIncrement exposing (PCIncrement(..))
import SimpleFlagOps exposing (singleByteFlagsCB)
import SingleNoParams exposing (ex_af, execute_0x76_halt, exx)
import Z80Core exposing (CoreChange(..), RareCoreChange(..), RepeatPCOffset(..), Z80Core)
import Z80CoreWithClockTime exposing (Z80, Z80CoreWithClockTime, di_0xF3, ei_0xFB)
import Z80Env exposing (Z80Env, setMem, setMem16, z80_out, z80_push, z80env_constructor)
import Z80Execute exposing (DeltaWithChanges(..), apply_delta)
import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Mem exposing (getMem8, mem16, z80_pop)
import Z80OpCode exposing (fetchInstruction)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IntWithFlagsTimeAndPC, MainRegisters, MainWithIndexRegisters, Z80ROM)


constructor : Z80
constructor =
    let
        main =
            MainWithIndexRegisters 0 0 0 0 0 0 0

        alternate =
            MainRegisters 0 0 0 0 0

        flags =
            FlagRegisters 0 0 0 0 0

        interrupts =
            Interrupts.InterruptRegisters IM0 False 0 0

        time =
            reset_cpu_time
    in
    --Z80 z80env_constructor 0 main main_flags alternate alt_flags 0 interrupts
    Z80 (Z80CoreWithClockTime.Z80CoreWithClockTime (Z80Core z80env_constructor main flags interrupts) time 0) alternate flags IFF_0



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


executeAndApplyDelta : Int -> CpuTimeCTime -> IFFValue -> Z80ROM -> Z80CoreWithClockTime -> Z80CoreWithClockTime
executeAndApplyDelta opCode cpuClock iff rom48k z80clock =
    let
        z80_core =
            z80clock.core

        ( delta, clockTime, pc_inc ) =
            z80_core |> execute_delta cpuClock opCode rom48k z80clock.pc

        coreChange =
            delta |> apply_delta z80_core iff rom48k clockTime
    in
    z80_core |> applyCoreChange coreChange clockTime pc_inc z80clock.pc rom48k


applyCoreChange : CoreChange -> CpuTimeCTime -> PCIncrement -> Int -> Z80ROM -> Z80Core -> Z80CoreWithClockTime
applyCoreChange coreChange clockTime pc_inc pc rom48k z80_core =
    let
        rawPc =
            case pc_inc of
                IncrementByOne ->
                    pc + 1

                IncrementByTwo ->
                    pc + 2

                IncrementByThree ->
                    pc + 3

                IncrementByFour ->
                    pc + 4

        pcAfter =
            Bitwise.and rawPc 0xFFFF
    in
    case coreChange of
        CallWithPCAndDelay int shortDelay ->
            let
                env =
                    z80_core.env |> z80_push pcAfter clockTime
            in
            { core = { main = z80_core.main, env = env, flags = z80_core.flags, interrupts = z80_core.interrupts }, pc = int, clockTime = clockTime |> addExtraCpuTime shortDelay }

        SetStackPointer new_sp ->
            let
                env =
                    z80_core.env

                new_env =
                    { ram = env.ram, sp = new_sp, borderColour = env.borderColour }
            in
            { core = { main = z80_core.main, env = new_env, interrupts = z80_core.interrupts, flags = z80_core.flags }, clockTime = clockTime, pc = pcAfter }

        Push16BitValue int ->
            let
                env =
                    z80_core.env |> z80_push int clockTime
            in
            { core = { main = z80_core.main, env = env, flags = z80_core.flags, interrupts = z80_core.interrupts }, clockTime = clockTime, pc = pcAfter }

        MainOnly z80_main ->
            { core = { main = z80_main, env = z80_core.env, flags = z80_core.flags, interrupts = z80_core.interrupts }, clockTime = clockTime, pc = pcAfter }

        FlagsOnly z80_flags ->
            { core = { main = z80_core.main, env = z80_core.env, flags = z80_flags, interrupts = z80_core.interrupts }, clockTime = clockTime, pc = pcAfter }

        ChangeMainAndFlags z80_main z80_flags ->
            { core = { main = z80_main, env = z80_core.env, flags = z80_flags, interrupts = z80_core.interrupts }, clockTime = clockTime, pc = pcAfter }

        ChangeMainAndSP z80_main sp ->
            let
                z80env =
                    z80_core.env
            in
            { core =
                { main = z80_main
                , env = { ram = z80env.ram, sp = sp, borderColour = z80env.borderColour }
                , flags = z80_core.flags
                , interrupts = z80_core.interrupts
                }
            , clockTime = clockTime
            , pc = pcAfter
            }

        ChangeFlagsAndSP z80_flags sp ->
            let
                z80env =
                    z80_core.env
            in
            { core = { main = z80_core.main, env = { ram = z80env.ram, sp = sp, borderColour = z80env.borderColour }, flags = z80_flags, interrupts = z80_core.interrupts }, clockTime = clockTime, pc = pcAfter }

        PopIntoPC ->
            let
                env =
                    z80_core.env

                popped =
                    env |> z80_pop rom48k clockTime

                new_env =
                    { ram = env.ram, sp = popped.sp, borderColour = env.borderColour }
            in
            { core = { main = z80_core.main, flags = z80_core.flags, interrupts = z80_core.interrupts, env = new_env }, clockTime = popped.time, pc = popped.value16 }

        RareChange rareChange ->
            case rareChange of
                CoreOnly z80Core ->
                    { core = z80Core, clockTime = clockTime, pc = pcAfter }

                LooperNoOffset z80Core ->
                    { core = z80Core, clockTime = clockTime, pc = pcAfter }

                Z80OutChange portNum ->
                    let
                        env =
                            z80_core.env

                        ( z80env, newTime ) =
                            env |> z80_out portNum z80_core.flags.a clockTime
                    in
                    { core = { z80_core | env = z80env }, clockTime = newTime, pc = pcAfter }

                NewInterrupts interruptRegisters ->
                    { core = { z80_core | interrupts = interruptRegisters }, clockTime = clockTime, pc = pcAfter }

        MainWithOffsetAndDelay offset shortDelay z80_main ->
            { core = { main = z80_main, env = z80_core.env, flags = z80_core.flags, interrupts = z80_core.interrupts }, pc = (pcAfter + offset) |> Bitwise.and 0xFFFF, clockTime = clockTime |> addExtraCpuTime shortDelay }

        JumpOnlyPC int ->
            { core = z80_core, clockTime = clockTime, pc = int }

        JumpWithOffset offset ->
            { core = z80_core, clockTime = clockTime, pc = (pcAfter + offset) |> Bitwise.and 0xFFFF }

        LooperJumpBack z80Core ->
            { core = z80Core, clockTime = clockTime, pc = pc }

        LooperWithDelayJumpBack shortDelay z80Core ->
            { core = z80Core, clockTime = clockTime |> addExtraCpuTime shortDelay, pc = pc }

        NoCore ->
            { core = z80_core, clockTime = clockTime, pc = pcAfter }

        JumpOffsetWithDelay int shortDelay ->
            { core = z80_core, clockTime = clockTime |> addExtraCpuTime shortDelay, pc = (pcAfter + int) |> Bitwise.and 0xFFFF }

        SetMem8 address value ->
            let
                ( z80env, time ) =
                    z80_core.env |> setMem address value clockTime
            in
            { core = { main = z80_core.main, env = z80env, flags = z80_core.flags, interrupts = z80_core.interrupts }, clockTime = time, pc = pcAfter }

        SetMem8Flags address flags ->
            let
                ( z80env, time ) =
                    z80_core.env |> setMem address flags.value clockTime
            in
            { core = { main = z80_core.main, env = z80env, flags = flags.flags, interrupts = z80_core.interrupts }, clockTime = time, pc = pcAfter }

        SetMem16 address value ->
            let
                ( z80env, time ) =
                    z80_core.env |> setMem16 address value clockTime
            in
            { core = { main = z80_core.main, env = z80env, flags = z80_core.flags, interrupts = z80_core.interrupts }, clockTime = time, pc = pcAfter }


execute_delta : CpuTimeCTime -> Int -> Z80ROM -> Int -> Z80Core -> ( DeltaWithChanges, CpuTimeCTime, PCIncrement )
execute_delta instrTime opCode rom48k pc z80_core =
    --int v, c = env.m1(PC, IR|R++&0x7F);
    --PC = (char)(PC+1); time += 4;
    --switch(c) {
    case opCode of
        0xCB ->
            let
                param =
                    z80_core.env |> mem (Bitwise.and (pc + 1) 0xFFFF) instrTime rom48k
            in
            runSpecialBitManipCB param z80_core

        0xED ->
            let
                param =
                    z80_core.env |> mem (Bitwise.and (pc + 1) 0xFFFF) instrTime rom48k
            in
            runSpecialEDMisc param rom48k pc z80_core

        0xDD ->
            let
                param =
                    z80_core.env |> mem (Bitwise.and (pc + 1) 0xFFFF) instrTime rom48k
            in
            if param.value == 0xCB then
                let
                    ixcboffset =
                        z80_core.env |> mem (Bitwise.and (pc + 2) 0xFFFF) param.time rom48k

                    ixcbparam =
                        z80_core.env |> mem (Bitwise.and (pc + 3) 0xFFFF) ixcboffset.time rom48k
                in
                runSpecialIXCB ixcboffset.value ixcbparam rom48k z80_core

            else
                runIndexIX param rom48k pc z80_core

        0xFD ->
            let
                param =
                    z80_core.env |> mem (Bitwise.and (pc + 1) 0xFFFF) instrTime rom48k
            in
            if param.value == 0xCB then
                let
                    iycboffset =
                        z80_core.env |> mem (Bitwise.and (pc + 2) 0xFFFF) param.time rom48k

                    iycbparam =
                        z80_core.env |> mem (Bitwise.and (pc + 3) 0xFFFF) iycboffset.time rom48k
                in
                runSpecialIYCB iycboffset.value iycbparam rom48k z80_core

            else
                runIndexIY param rom48k pc z80_core

        _ ->
            case singleByteInstructions |> Dict.get opCode of
                Just ( mainRegFunc, duration ) ->
                    ( RegisterChangeDelta mainRegFunc, instrTime |> addDuration duration, IncrementByOne )

                Nothing ->
                    case twoByteInstructions |> Dict.get opCode of
                        Just ( f, duration ) ->
                            let
                                paramTimeValue =
                                    z80_core.env |> mem (Bitwise.and (pc + 1) 0xFFFF) instrTime rom48k
                            in
                            ( TwoByteDelta (f paramTimeValue.value), paramTimeValue.time |> addDuration duration, IncrementByTwo )

                        Nothing ->
                            case threeByteInstructions |> Dict.get opCode of
                                Just ( f, duration ) ->
                                    let
                                        env =
                                            z80_core.env

                                        doubleParam =
                                            env |> mem16 (Bitwise.and (pc + 1) 0xFFFF) rom48k instrTime
                                    in
                                    ( ThreeBytePlainDelta (f doubleParam.value16), doubleParam.time |> addDuration duration, IncrementByThree )

                                Nothing ->
                                    ( UnknownInstruction "runOrdinary" opCode, instrTime, IncrementByOne )


runIndexIX : Int -> CpuTimeCTime -> Z80ROM -> Int -> Z80Core -> ( DeltaWithChanges, CpuTimeCTime, PCIncrement )
runIndexIX param clockTime rom48k pc z80 =
    case singleByteMainFlagsRegsIX |> Dict.get param of
        Just ( mainRegFunc, duration ) ->
            ( RegisterChangeDelta mainRegFunc, clockTime |> addDuration duration, IncrementByTwo )

        Nothing ->
            case twoByteWithRegistersIX |> Dict.get param of
                Just ( f, duration ) ->
                    let
                        time =
                            clockTime |> addDuration duration

                        ( doubleParam, dTimeValue ) =
                            z80.env |> getMem8 (Bitwise.and (pc + 2) 0xFFFF) time rom48k
                    in
                    ( DoubleWithRegistersDelta (f doubleParam), dTimeValue, IncrementByThree )

                Nothing ->
                    case threeByteWithRegistersIX |> Dict.get param of
                        Just ( f, duration ) ->
                            let
                                env_1 =
                                    clockTime |> addDuration duration

                                doubleParam =
                                    z80.env |> mem16 (Bitwise.and (pc + 2) 0xFFFF) rom48k env_1
                            in
                            ( Triple16ParamDelta (f doubleParam.value16), doubleParam.time, IncrementByFour )

                        Nothing ->
                            ( UnknownInstruction "execute IndexIX" param, clockTime, IncrementByTwo )


runIndexIY : Int -> CpuTimeCTime -> Z80ROM -> Int -> Z80Core -> ( DeltaWithChanges, CpuTimeCTime, PCIncrement )
runIndexIY param clockTime rom48k pc z80 =
    case singleByteMainFlagsRegsIY |> Dict.get param of
        Just ( mainRegFunc, duration ) ->
            ( RegisterChangeDelta mainRegFunc, clockTime |> addDuration duration, IncrementByTwo )

        Nothing ->
            case twoByteWithRegistersIY |> Dict.get param of
                Just ( f, duration ) ->
                    let
                        time =
                            clockTime |> addDuration duration

                        ( doubleParam, dClockTime ) =
                            z80.env |> getMem8 (Bitwise.and (pc + 2) 0xFFFF) time rom48k
                    in
                    ( DoubleWithRegistersDelta (f doubleParam), dClockTime, IncrementByThree )

                Nothing ->
                    case threeByteWithRegistersIY |> Dict.get param of
                        Just ( f, duration ) ->
                            let
                                env =
                                    z80.env

                                env_1 =
                                    clockTime |> addDuration duration

                                doubleParam =
                                    env |> mem16 (Bitwise.and (pc + 2) 0xFFFF) rom48k env_1
                            in
                            ( Triple16ParamDelta (f doubleParam.value16), doubleParam.time, IncrementByFour )

                        Nothing ->
                            ( UnknownInstruction "execute IndexIY" param, clockTime, IncrementByTwo )


runSpecialBitManipCB : CpuTimeAndValue -> Z80Core -> ( DeltaWithChanges, CpuTimeCTime, PCIncrement )
runSpecialBitManipCB param z80_core =
    case singleByteMainRegsCB |> Dict.get param.value of
        Just ( mainRegFunc, duration ) ->
            ( RegisterChangeDelta (mainRegFunc z80_core.main), param.time |> addDuration duration, IncrementByTwo )

        Nothing ->
            case singleByteMainAndFlagRegistersCB |> Dict.get param of
                Just ( f, duration ) ->
                    ( PureDelta (f z80_core.main z80_core.flags), clockTime |> addDuration duration, IncrementByTwo )

                Nothing ->
                    case singleByteFlagsCB |> Dict.get param of
                        Just ( flagFunc, duration ) ->
                            ( RegisterChangeDelta flagFunc, clockTime |> addDuration duration, IncrementByTwo )

                        Nothing ->
                            case singleEnvMainRegsCB |> Dict.get param of
                                Just ( f, duration ) ->
                                    ( MainWithEnvDelta (f z80_core.main), clockTime |> addDuration duration, IncrementByTwo )

                                Nothing ->
                                    ( UnknownInstruction "execute CB" param, clockTime, IncrementByTwo )


runSpecialIXCB : Int -> CpuTimeAndValue -> Z80ROM -> Z80Core -> ( DeltaWithChanges, CpuTimeCTime, PCIncrement )
runSpecialIXCB offset param rom48k z80_core =
    case singleByteMainRegsIXCB |> Dict.get param.value of
        Just ( mainRegFunc, duration ) ->
            ( RegisterChangeDelta (mainRegFunc offset z80_core.main), param.time |> addDuration duration, IncrementByFour )

        Nothing ->
            case singleEnvMainRegsIXCB |> Dict.get param of
                Just ( f, duration ) ->
                    ( MainWithEnvDelta (f z80_core.main offset rom48k z80_core.env), clockTime |> addDuration duration, IncrementByFour )

                Nothing ->
                    ( UnknownInstruction "execute IXCB" param, clockTime, IncrementByFour )


runSpecialIYCB : Int -> CpuTimeAndValue -> Z80ROM -> Z80Core -> ( DeltaWithChanges, CpuTimeCTime, PCIncrement )
runSpecialIYCB iycboffset param rom48k z80_core =
    case singleByteMainRegsIYCB |> Dict.get param.value |> Maybe.map (\( f, d ) -> ( f iycboffset, d )) of
        Just ( mainRegFunc, duration ) ->
            ( RegisterChangeDelta (mainRegFunc z80_core.main), param.time |> addDuration duration, IncrementByFour )

        Nothing ->
            case singleEnvMainRegsIYCB |> Dict.get param of
                Just ( f, duration ) ->
                    ( MainWithEnvDelta (f z80_core.main iycboffset rom48k z80_core.env), clockTime |> addDuration duration, IncrementByFour )

                Nothing ->
                    ( UnknownInstruction "execute IYCB" param, clockTime, IncrementByFour )


runSpecialEDMisc : CpuTimeAndValue -> Z80ROM -> Int -> Z80Core -> ( DeltaWithChanges, CpuTimeCTime, PCIncrement )
runSpecialEDMisc param rom48k pc z80_core =
    case singleByteMainRegsED |> Dict.get param.value of
        Just ( mainRegFunc, duration ) ->
            ( EDChangeDelta mainRegFunc, param.time |> addDuration duration, IncrementByTwo )

        Nothing ->
            case singleByteFlagsED |> Dict.get param of
                Just ( flagFunc, duration ) ->
                    ( RegisterChangeDelta (flagFunc z80_core.flags), clockTime |> addDuration duration, IncrementByTwo )

                Nothing ->
                    case singleByteMainAndFlagsED |> Dict.get param of
                        Just ( f, pcInc, duration ) ->
                            ( PureDelta (f z80_core.main z80_core.flags), clockTime |> addDuration duration, pcInc )

                        Nothing ->
                            case edWithInterrupts |> Dict.get param of
                                Just ( f, duration ) ->
                                    ( InterruptDelta (f z80_core.interrupts), clockTime |> addDuration duration, IncrementByTwo )

                                Nothing ->
                                    case fourByteMainED |> Dict.get param of
                                        Just ( mainRegFunc, duration ) ->
                                            let
                                                doubleParam =
                                                    z80_core.env |> mem16 (Bitwise.and (pc + 2) 0xFFFF) rom48k clockTime
                                            in
                                            ( EDFourByteDelta (mainRegFunc z80_core.main doubleParam.value16), doubleParam.time |> addDuration duration, IncrementByFour )

                                        Nothing ->
                                            ( UnknownInstruction "runSpecial" param, clockTime, IncrementByTwo )



-- Only used in tests


executeCoreInstruction : Z80ROM -> Int -> Z80Core -> ( Z80Core, CpuTimeCTime, Int )
executeCoreInstruction rom48k pc z80_core =
    let
        ct =
            z80_core |> fetchInstruction pc rom48k reset_cpu_time 0

        clock =
            { core = z80_core, pc = pc, clockTime = reset_cpu_time }

        newClock =
            clock |> executeAndApplyDelta ct.value ct.time IFF_0 rom48k
    in
    ( newClock.core, newClock.clockTime, newClock.pc )


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



--nonCoreOpCodeSet =
--    nonCoreOpCodeList |> Set.fromList


isCoreOpCode : Int -> Bool
isCoreOpCode value =
    --nonCoreOpCodeSet |> Set.member value |> not
    --value /= c_EX_AF_AFDASH && value /= c_EXX && value /= c_HALT && value /= c_DI && value /= c_EI
    -- This is still much faster than the set membership or the custom check
    nonCoreOpCodeList |> List.member value |> not


stillLooping : Z80CoreWithClockTime -> Bool
stillLooping z80core =
    c_TIME_LIMIT > z80core.clockTime.cpu_time


coreLooping : ( Z80CoreWithClockTime, CpuTimeAndValue, Int ) -> Bool
coreLooping ( z80core, timeAndValue, _ ) =
    isCoreOpCode timeAndValue.value && (z80core |> stillLooping)


executeCore : Z80ROM -> Z80 -> Z80
executeCore rom48k z80 =
    let
        z80_clock =
            z80.coreWithClock

        z80_core =
            z80_clock.core

        execute_f =
            \( clock, ct, r_register ) ->
                let
                    core_1_clock =
                        clock |> executeAndApplyDelta ct.value ct.time z80.iff rom48k
                in
                ( core_1_clock, fetchInstruction core_1_clock.pc rom48k core_1_clock.clockTime r_register core_1_clock.core, r_register + 1 )

        ( clock_2, ct1, new_r ) =
            Loop.while coreLooping execute_f ( z80_clock, fetchInstruction z80_clock.pc rom48k z80_clock.clockTime z80_clock.core.interrupts.r z80_clock.core, z80_core.interrupts.r )

        core_2 =
            clock_2.core

        core_ints =
            core_2.interrupts

        z80_1 =
            { z80 | coreWithClock = { clock_2 | core = { core_2 | interrupts = { core_ints | r = new_r } } } }
    in
    case nonCoreFuncs |> Dict.get ct1.value of
        Just ( f, duration ) ->
            let
                z80_2 =
                    z80_1 |> f

                clock =
                    z80_2.coreWithClock

                core =
                    clock.core

                ints =
                    core.interrupts

                newTime =
                    clock.clockTime |> addDuration duration

                pc =
                    Bitwise.and (clock.pc + 1) 0xFFFF
            in
            { z80_2 | coreWithClock = { clock | pc = pc, clockTime = newTime, core = { core | interrupts = { ints | r = ints.r + 1 } } } }

        Nothing ->
            z80_1


execute : Z80ROM -> Z80 -> Z80
execute rom48k z80 =
    let
        z80_clock =
            z80.coreWithClock

        z80_core =
            z80_clock.core
    in
    if z80_core.interrupts.halted then
        z80 |> execute_0x76_halt

    else
        let
            execute_f =
                executeCore rom48k
        in
        Loop.while (\x -> stillLooping x.coreWithClock) execute_f z80



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
