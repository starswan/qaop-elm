--
-- $Id$
--


module Z80 exposing (..)

import Array exposing (Array)
import Bitwise exposing (and, or)
import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeAndValue, CpuTimeCTime, CpuTimePcAndValue, addDuration)
import Dict exposing (Dict)
import Group0x30 exposing (delta_dict_lite_30)
import Group0xE0 exposing (delta_dict_lite_E0)
import Group0xF0 exposing (list0255, lt40_array, xYDict)
import Loop
import PCIncrement exposing (MediumPCIncrement(..))
import SimpleFlagOps exposing (singleByteFlags)
import SimpleSingleByte exposing (singleByteMainRegs)
import SingleByteWithEnv exposing (singleByteZ80Env)
import SingleEnvWithMain exposing (singleEnvMainRegs)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters)
import SingleNoParams exposing (singleWithNoParam)
import SingleWith8BitParameter exposing (doubleWithRegisters, maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (triple16WithFlags)
import TripleWithMain exposing (tripleMainRegs)
import Utils exposing (toHexString)
import Z80Debug exposing (debugTodo)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..))
import Z80Env exposing (Z80Env, c_TIME_LIMIT, m1, mem, mem16, z80env_constructor)
import Z80Execute exposing (DeltaWithChanges(..), apply_delta)
import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL(..), IntWithFlagsTimeAndPC, InterruptRegisters, MainRegisters, MainWithIndexRegisters, Z80, add_cpu_time, inc_pc, z80_halt)


constructor : Z80
constructor =
    let
        main =
            Z80Types.MainWithIndexRegisters 0 0 0 0 0 0 0

        alternate =
            MainRegisters 0 0 0 0 0

        main_flags =
            FlagRegisters 0 0 0 0 0

        alt_flags =
            FlagRegisters 0 0 0 0 0

        interrupts =
            InterruptRegisters 0 0 0 False
    in
    --Z80 z80env_constructor 0 main main_flags alternate alt_flags interrupts (c_FRSTART + c_FRTIME)
    Z80 z80env_constructor 0 main main_flags alternate alt_flags 0 interrupts



--	int a() {return A;}
--get_a: Z80 -> Int
--get_a z80 =
--    z80.flags.a
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
--	private void cpir(int i, boolean r)
--	{
--		int a,b,v;
--
--		v = A-(b = env.mem(a=HL)) & 0xFF;
--		MP += i;
--		HL = (char)(a+i);
--		time += 8;
--
--		Fr = v & 0x7F | v>>>7;
--		Fb = ~(b | 0x80);
--		Fa = A & 0x7F;
--
--		bc(a = (char)(bc() - 1));
--		if(a!=0) {
--			Fa |= 0x80;
--			Fb |= 0x80;
--			if(r && v!=0) {
--				MP = (PC = (char)(PC-2)) + 1;
--				time += 5;
--			}
--		}
--
--		Ff = Ff&~0xFF | v&~F53;
--		if(((v ^ b ^ A)&FH) != 0) v--;
--		Ff |= v<<4&0x20 | v&8;
--	}
--
--	private void inir_otir(int op) // op: 101rd01o
--	{
--		int bc, hl, d, v;
--
--		hl = (char)(HL + (d = (op&8)==0 ? 1 : -1));
--		bc = B<<8|C;
--		time++;
--		if((op&1)==0) {
--			v = env.in(bc); time += 4;
--			MP = bc+d;
--			bc = (char)(bc-256);
--			env.mem(HL, v); time += 3;
--			d += bc;
--		} else {
--			v = env.mem(HL); time += 3;
--			bc = (char)(bc-256);
--			MP = bc+d;
--			env.out(bc, v); time += 4;
--			d = hl;
--		}
--		d = (d&0xFF) + v;
--		HL = hl;
--		B = (bc >>= 8);
--		if(op>0xB0 && bc>0) {
--			time += 5;
--			PC = (char)(PC-2);
--		}
--		int x = d&7 ^ bc;
--		Ff = bc | (d &= 0x100);
--		Fa = (Fr = bc) ^ 0x80;
--		x = 0x4B3480 >> ((x^x>>>4)&15);
--		Fb = (x^bc) & 0x80 | d>>>4 | (v & 0x80)<<2;
--	}
--
--	/* Note: EI isn't prefix here - interrupt will be acknowledged */
--
--toString: IXIYHL -> String
--toString ixiyhl =
--   case ixiyhl of
--      IX -> "IX"
--      IY -> "IY"
--      HL -> "HL"


execute_ltC0 : Int -> Z80ROM -> Z80 -> Maybe Z80Delta
execute_ltC0 c rom48k z80 =
    case lt40_array |> Array.get c |> Maybe.withDefault Nothing of
        Just f ->
            Just (z80 |> f HL rom48k)

        Nothing ->
            case lt40_array_lite |> Array.get c |> Maybe.withDefault Nothing of
                Just f_without_ixiyhl ->
                    Just (z80 |> f_without_ixiyhl rom48k)

                Nothing ->
                    Nothing


execute_ltC0_xy : Int -> IXIY -> Z80ROM -> Z80 -> Maybe Z80Delta
execute_ltC0_xy c ixoriy rom48k z80 =
    case xYDict |> Dict.get c of
        Just xyFunc ->
            Just (z80 |> xyFunc ixoriy rom48k)

        Nothing ->
            let
                only_ixiy =
                    case ixoriy of
                        IXIY_IX ->
                            IX

                        IXIY_IY ->
                            IY
            in
            case lt40_array |> Array.get c |> Maybe.withDefault Nothing of
                Just f ->
                    Just (z80 |> f only_ixiy rom48k)

                Nothing ->
                    case lt40_array_lite |> Array.get c |> Maybe.withDefault Nothing of
                        Just f_without_ixiyhl ->
                            Just (z80 |> f_without_ixiyhl rom48k)

                        Nothing ->
                            Nothing



--z80_to_delta: Maybe (Z80ROM -> Z80 -> Z80) -> Maybe (Z80ROM -> Z80 -> Z80Delta)
--z80_to_delta z80func =
--    case z80func of
--        Just f ->  Just (\rom48k z80  -> Whole (z80 |> f rom48k))
--        Nothing -> Nothing
--mergeFuncList:  Maybe (Z80ROM -> Z80 -> Z80Delta) -> Maybe (Z80ROM -> Z80 -> Z80Delta) -> Maybe (Z80ROM -> Z80 -> Z80Delta)
--mergeFuncList afunc bfunc =
--    case afunc of
--        Just a -> Just a
--        Nothing -> case bfunc of
--                        Just b -> Just b
--                        Nothing -> Nothing


lt40_array_lite : Array (Maybe (Z80ROM -> Z80 -> Z80Delta))
lt40_array_lite =
    let
        --z80_funcs = []
        delta_funcs =
            list0255 |> List.map (\index -> lt40_delta_dict_lite |> Dict.get index)
    in
    --List.map2 mergeFuncList z80_funcs delta_funcs |> Array.fromList
    delta_funcs |> Array.fromList


lt40_delta_dict_lite : Dict Int (Z80ROM -> Z80 -> Z80Delta)
lt40_delta_dict_lite =
    Dict.fromList
        [ ( 0xDD, \z80 -> group_xy IXIY_IX z80 )
        , ( 0xFD, \z80 -> group_xy IXIY_IY z80 )
        ]
        |> Dict.union delta_dict_lite_30
        |> Dict.union delta_dict_lite_E0



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


execute_delta : CpuTimeAndValue -> Z80ROM -> Z80 -> DeltaWithChanges
execute_delta ct rom48k z80 =
    --int v, c = env.m1(PC, IR|R++&0x7F);
    --PC = (char)(PC+1); time += 4;
    --switch(c) {
    let
        ( instrCode, instrTime, paramOffset ) =
            case ct.value of
                0xCB ->
                    let
                        param =
                            mem (Bitwise.and (z80.pc + 1) 0xFFFF) ct.time rom48k z80.env.ram
                    in
                    ( Bitwise.or 0xCB00 param.value, param.time, 1 )

                0xDD ->
                    let
                        param =
                            mem (Bitwise.and (z80.pc + 1) 0xFFFF) ct.time rom48k z80.env.ram
                    in
                    ( Bitwise.or 0xDD00 param.value, param.time, 2 )

                0xFD ->
                    let
                        param =
                            mem (Bitwise.and (z80.pc + 1) 0xFFFF) ct.time rom48k z80.env.ram
                    in
                    ( Bitwise.or 0xFD00 param.value, param.time, 2 )

                _ ->
                    ( ct.value, ct.time, 1 )
    in
    case singleByteMainRegs |> Dict.get instrCode of
        Just ( mainRegFunc, t, duration ) ->
            RegisterChangeDelta t duration (mainRegFunc z80.main)

        Nothing ->
            case singleByteFlags |> Dict.get instrCode of
                Just ( flagFunc, t, duration ) ->
                    FlagDelta t duration (flagFunc z80.flags)

                Nothing ->
                    case tripleByteWith16BitParam |> Dict.get instrCode of
                        Just ( f, pcInc, duration ) ->
                            let
                                env =
                                    z80.env

                                env_1 =
                                    { env | time = instrTime |> addDuration duration }

                                doubleParam =
                                    env_1 |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
                            in
                            Triple16ParamDelta doubleParam.time pcInc (f doubleParam.value16)

                        Nothing ->
                            case maybeRelativeJump |> Dict.get instrCode of
                                Just ( f, duration ) ->
                                    let
                                        newTime =
                                            instrTime |> addDuration duration

                                        param =
                                            mem (Bitwise.and (z80.pc + 1) 0xFFFF) newTime rom48k z80.env.ram
                                    in
                                    JumpChangeDelta param.time (f param.value z80.flags)

                                Nothing ->
                                    case singleEnvMainRegs |> Dict.get instrCode of
                                        Just ( f, pcInc, duration ) ->
                                            MainWithEnvDelta pcInc duration (f z80.main rom48k z80.env)

                                        Nothing ->
                                            case triple16WithFlags |> Dict.get instrCode of
                                                Just ( f, duration ) ->
                                                    let
                                                        env =
                                                            z80.env

                                                        env_1 =
                                                            { env | time = env.time |> addDuration duration }

                                                        doubleParam =
                                                            env_1 |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
                                                    in
                                                    Triple16FlagsDelta doubleParam.time (f doubleParam.value16 z80.flags)

                                                Nothing ->
                                                    case singleByteMainAndFlagRegisters |> Dict.get instrCode of
                                                        Just ( f, pcInc, duration ) ->
                                                            PureDelta pcInc (instrTime |> addDuration duration) (f z80.main z80.flags)

                                                        Nothing ->
                                                            case doubleWithRegisters |> Dict.get instrCode of
                                                                Just ( f, pcInc, duration ) ->
                                                                    let
                                                                        time =
                                                                            instrTime |> addDuration duration

                                                                        param =
                                                                            case pcInc of
                                                                                IncreaseByTwo ->
                                                                                    mem (Bitwise.and (z80.pc + 1) 0xFFFF) time rom48k z80.env.ram

                                                                                IncreaseByThree ->
                                                                                    mem (Bitwise.and (z80.pc + 2) 0xFFFF) time rom48k z80.env.ram
                                                                    in
                                                                    DoubleWithRegistersDelta pcInc param.time (f z80.main param.value)

                                                                Nothing ->
                                                                    case tripleMainRegs |> Dict.get instrCode of
                                                                        Just ( f, pcInc, duration ) ->
                                                                            let
                                                                                time =
                                                                                    instrTime |> addDuration duration

                                                                                env =
                                                                                    z80.env

                                                                                doubleParam =
                                                                                    { env | time = time } |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
                                                                            in
                                                                            TripleMainChangeDelta doubleParam.time pcInc (f doubleParam.value16 z80.main)

                                                                        Nothing ->
                                                                            case singleByteZ80Env |> Dict.get instrCode of
                                                                                Just ( f, duration ) ->
                                                                                    SingleEnvDelta (instrTime |> addDuration duration) (f z80.env)

                                                                                Nothing ->
                                                                                    case singleWithNoParam |> Dict.get instrCode of
                                                                                        Just ( f, duration ) ->
                                                                                            NoParamsDelta (instrTime |> addDuration duration) f

                                                                                        Nothing ->
                                                                                            case singleByte instrTime instrCode z80 rom48k of
                                                                                                Just deltaThing ->
                                                                                                    deltaThing

                                                                                                Nothing ->
                                                                                                    oldDelta ct z80.interrupts z80 rom48k



-- case 0xD4: call((Ff&0x100)==0); break;
-- case 0xE4: call((flags()&FP)==0); break;
-- case 0xEC: call((flags()&FP)!=0); break;
-- case 0xF4: call((Ff&FS)==0); break;
-- case 0xFC: call((Ff&FS)!=0); break;
-- case 0xF3: IFF=0; break;


singleByte : CpuTimeCTime -> Int -> Z80 -> Z80ROM -> Maybe DeltaWithChanges
singleByte ctime instr_code tmp_z80 rom48k =
    case singleWith8BitParam |> Dict.get instr_code of
        Just ( f, pcInc, duration ) ->
            let
                instrTime =
                    ctime |> addDuration duration

                param =
                    case pcInc of
                        IncreaseByTwo ->
                            mem (Bitwise.and (tmp_z80.pc + 1) 0xFFFF) instrTime rom48k tmp_z80.env.ram

                        IncreaseByThree ->
                            mem (Bitwise.and (tmp_z80.pc + 2) 0xFFFF) instrTime rom48k tmp_z80.env.ram
            in
            Just (Simple8BitDelta pcInc param.time (f param.value))

        Nothing ->
            Nothing


oldDelta : CpuTimeAndValue -> InterruptRegisters -> Z80 -> Z80ROM -> DeltaWithChanges
oldDelta c interrupts tmp_z80 rom48k =
    let
        env =
            tmp_z80.env

        old_z80 =
            { tmp_z80 | env = { env | time = c.time }, r = tmp_z80.r + 1 }

        new_pc =
            Bitwise.and (old_z80.pc + 1) 0xFFFF

        z80 =
            { old_z80 | pc = new_pc } |> add_cpu_time 4

        new_time =
            z80.env.time
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
    case z80 |> execute_ltC0 c.value rom48k of
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
                    debugTodo "execute" (c.value |> toHexString) z80 |> Whole
            in
            OldDeltaWithChanges (DeltaWithChangesData delta interrupts new_pc new_time)


executeSingleInstruction : Z80ROM -> Z80 -> Z80
executeSingleInstruction rom48k z80 =
    let
        ct =
            z80.env |> m1 z80.pc (Bitwise.or z80.interrupts.ir (Bitwise.and z80.r 0x7F)) rom48k
    in
    z80 |> execute_delta ct rom48k |> apply_delta z80 rom48k


execute : Z80ROM -> Z80 -> Z80
execute rom48k z80 =
    if z80.interrupts.halted then
        z80_halt z80

    else
        let
            execute_f =
                executeSingleInstruction rom48k
        in
        Loop.while (\x -> c_TIME_LIMIT > x.env.time.cpu_time) execute_f z80



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


group_xy : IXIY -> Z80ROM -> Z80 -> Z80Delta
group_xy ixiy rom48k old_z80 =
    let
        c =
            old_z80.env |> m1 old_z80.pc (or old_z80.interrupts.ir (and old_z80.r 0x7F)) rom48k

        --intr = old_z80.interrupts
        env =
            old_z80.env

        z80_1 =
            { old_z80 | env = { env | time = c.time }, r = old_z80.r + 1 }

        new_pc =
            z80_1 |> inc_pc

        z80 =
            { z80_1 | pc = new_pc } |> add_cpu_time 4

        ltc0 =
            z80 |> execute_ltC0_xy c.value ixiy rom48k
    in
    case ltc0 of
        Just z_z80 ->
            z_z80

        Nothing ->
            --case c.value of
            --0xCB -> group_xy_cb ixiy z80 |> Whole
            --_ -> debug_todo "group_xy" (c.value |> toHexString) z80 |> Whole
            debugTodo "group_xy" (c.value |> toHexString) z80 |> Whole



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
