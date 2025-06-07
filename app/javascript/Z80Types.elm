module Z80Types exposing (..)

import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, CpuTimePcAnd16BitValue, CpuTimePcAndValue, CpuTimePcAndZ80Byte, addCpuTimeTime)
import Z80Byte exposing (Z80Byte)
import Z80Env exposing (Z80Env, Z80EnvWithPC, mem)
import Z80Flags exposing (FlagRegisters)
import Z80Ram exposing (Z80Ram)
import Z80Rom exposing (Z80ROM)
import Z80Word exposing (Z80Word, incrementBy1, lower8Bits, top8Bits)


type alias MainRegisters =
    { b : Z80Byte
    , c : Z80Byte
    , d : Z80Byte
    , e : Z80Byte
    , hl : Z80Word
    }


type alias MainWithIndexRegisters =
    { b : Z80Byte
    , c : Z80Byte
    , d : Z80Byte
    , e : Z80Byte
    , hl : Z80Word
    , ix : Z80Word
    , iy : Z80Word
    }


type alias InterruptRegisters =
    { ir : Z80Word
    , --mp:  Int, -- /* MEMPTR, the hidden register emulated according to memptr_eng.txt */
      iff : Int
    , iM : Int
    , halted : Bool
    }


type alias EnvWithPCAndValue =
    { env : Z80Env
    , pc : Int
    , value : Int
    }


type alias IntWithFlagsTimeAndPC =
    { value : Int
    , flags : FlagRegisters
    , time : CpuTimeCTime
    , pc : Int
    }


type alias Z80PushValue =
    { value : Int
    }



-- Think this could be a useful parameter to execute_lt40 to avoid the duplication
-- problem currently being experienced in function group_xy


type IXIYHL
    = IX
    | IY
    | HL


type IXIY
    = IXIY_IX
    | IXIY_IY



--
--	private int imm8()
--	{
--		int v = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 3;
--		return v;
--	}


imm8 : Z80Word -> CpuTimeCTime -> Z80ROM -> Z80Ram -> CpuTimePcAndZ80Byte
imm8 pc time rom48k ram =
    let
        v =
            mem pc time rom48k ram

        new_pc =
            pc |> incrementBy1

        env_1 =
            v.time |> addCpuTimeTime 3
    in
    CpuTimePcAndZ80Byte env_1 new_pc v.value



--	private void jp(boolean y)
--	{
--		int a = MP = imm16();
--		if(y) PC = a;
--	}
--jp_z80 : Bool -> Z80ROM -> Z80 -> Z80
--jp_z80 y rom48k z80 =
--    let
--        a =
--            z80 |> imm16 rom48k
--
--        env =
--            z80.env
--
--        z80_1 =
--            { z80 | pc = a.pc, env = { env | time = a.time } }
--    in
--    if y then
--        { z80_1 | pc = a.value }
--
--    else
--        z80_1
--	private void call(boolean y)
--	{
--		int a = MP = imm16();
--		if(y) {push(PC); PC = a;}
--	}
--call_if : Bool -> Z80ROM -> Z80 -> Z80EnvWithPC
--call_if y rom48k z80 =
--    let
--        a =
--            z80 |> imm16 rom48k
--
--        env =
--            z80.env
--
--        --z80_2 =
--        --    { z80 | pc = a.pc, env = { env | time = a.time } }
--        new_env =
--            { env | time = a.time }
--    in
--    if y then
--        let
--            --b = debug_log "call" (a.value |> subName) Nothing
--            --z80_1 = z80_2 |> push z80_2.pc |> set_pc a.value
--            pushed =
--                new_env |> z80_push a.pc
--
--            --z80_1 = { z80_2 | env = pushed, pc = a.value }
--        in
--        Z80EnvWithPC pushed a.value
--
--    else
--        Z80EnvWithPC new_env a.pc
--rst_z80 : Int -> Z80 -> Z80
--rst_z80 c z80 =
--    --z80 |> push z80.pc |> set_pc (c - 199)
--    let
--        pushed =
--            z80.env |> z80_push z80.pc
--    in
--    { z80 | env = pushed, pc = c - 199 }


get_ixiy_xy : IXIY -> MainWithIndexRegisters -> Z80Word
get_ixiy_xy ixiy z80_main =
    case ixiy of
        IXIY_IX ->
            z80_main.ix

        IXIY_IY ->
            z80_main.iy


set_h : Z80Byte -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_h value ixiyhl z80 =
    let
        xy =
            get_xy ixiyhl z80
    in
    --set_xy (Bitwise.or (Bitwise.and xy 0xFF) (value |> z80byteToInt |> shiftLeftBy8)) ixiyhl z80
    set_xy (Z80Word (lower8Bits xy) value) ixiyhl z80


set_h_ixiy : Z80Byte -> IXIY -> MainWithIndexRegisters -> MainWithIndexRegisters
set_h_ixiy value ixiyhl z80 =
    let
        xy =
            get_xy_ixiy ixiyhl z80
    in
    --set_xy_ixiy (Bitwise.or (Bitwise.and xy 0xFF) (shiftLeftBy8 value)) ixiyhl z80
    set_xy_ixiy (Z80Word (lower8Bits xy) value) ixiyhl z80


set_l : Z80Byte -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_l value ixiyhl z80 =
    let
        xy =
            get_xy ixiyhl z80
    in
    --set_xy (Bitwise.or (Bitwise.and xy 0xFF00) (value |> z80byteToInt)) ixiyhl z80
    set_xy (Z80Word value (top8Bits xy)) ixiyhl z80


set_l_ixiy : Z80Byte -> IXIY -> MainWithIndexRegisters -> MainWithIndexRegisters
set_l_ixiy value ixiyhl z80 =
    let
        xy =
            get_xy_ixiy ixiyhl z80
    in
    --set_xy_ixiy (Bitwise.or (Bitwise.and xy 0xFF00) value) ixiyhl z80
    set_xy_ixiy (Z80Word value (top8Bits xy)) ixiyhl z80


get_xy : IXIYHL -> MainWithIndexRegisters -> Z80Word
get_xy ixiyhl z80_main =
    case ixiyhl of
        IX ->
            z80_main.ix

        IY ->
            z80_main.iy

        HL ->
            z80_main.hl


get_xy_ixiy : IXIY -> MainWithIndexRegisters -> Z80Word
get_xy_ixiy ixiyhl z80_main =
    case ixiyhl of
        IXIY_IX ->
            z80_main.ix

        IXIY_IY ->
            z80_main.iy


set_xy : Z80Word -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_xy value ixiyhl z80 =
    case ixiyhl of
        IX ->
            { z80 | ix = value }

        IY ->
            { z80 | iy = value }

        HL ->
            { z80 | hl = value }


set_xy_ixiy : Z80Word -> IXIY -> MainWithIndexRegisters -> MainWithIndexRegisters
set_xy_ixiy value ixiyhl z80 =
    case ixiyhl of
        IXIY_IX ->
            { z80 | ix = value }

        IXIY_IY ->
            { z80 | iy = value }



--
--	private int getd(int xy)
--	{
--		int d = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 8;
--		return MP = (char)(xy + (byte)d);
--	}


get_bc : MainWithIndexRegisters -> Z80Word
get_bc z80_main =
    --z80_main.b |> z80ToInt |> shiftLeftBy8 |> Bitwise.or (z80_main.c |> z80ToInt)
    Z80Word z80_main.c z80_main.b


get_de : MainWithIndexRegisters -> Z80Word
get_de z80_main =
    --z80_main.d |> z80ToInt |> shiftLeftBy8 |> Bitwise.or (z80_main.e |> z80ToInt)
    Z80Word z80_main.e z80_main.d


set_bc_main : Z80Word -> MainWithIndexRegisters -> MainWithIndexRegisters
set_bc_main v z80_main =
    { z80_main | b = v.high, c = v.low }


set_de_main : Z80Word -> MainWithIndexRegisters -> MainWithIndexRegisters
set_de_main v z80_main =
    { z80_main | d = v.high, e = v.low }



--	private void jr()
--	{
--		int pc = PC;
--		byte d = (byte)env.mem(pc); time += 8;
--		MP = PC = (char)(pc+d+1);
--	}
--jr : Z80ROM -> Z80 -> CpuTimeAndPc
--jr rom48k z80 =
--    let
--        mempc =
--            mem z80.pc z80.env.time rom48k z80.env.ram
--
--        d =
--            byte mempc.value
--
--        --x = Debug.log "jr" ((String.fromInt d.value) ++ " " ++ (String.fromInt (byte d.value)))
--    in
--    --z80 |> set_env mempc.env |> add_cpu_time 8 |> set_pc (z80.pc + d + 1)
--    CpuTimeAndPc (mempc.time |> addCpuTimeTime 8) (Bitwise.and (z80.pc + d + 1) 0xFFFF)


get_h : IXIYHL -> MainWithIndexRegisters -> Z80Byte
get_h ixiyhl z80 =
    top8Bits (get_xy ixiyhl z80)


get_h_ixiy : IXIY -> MainWithIndexRegisters -> Z80Byte
get_h_ixiy ixiyhl z80 =
    top8Bits (get_xy_ixiy ixiyhl z80)


get_l : IXIYHL -> MainWithIndexRegisters -> Z80Byte
get_l ixiyhl z80 =
    lower8Bits (get_xy ixiyhl z80)


get_l_ixiy : IXIY -> MainWithIndexRegisters -> Z80Byte
get_l_ixiy ixiyhl z80 =
    lower8Bits (get_xy_ixiy ixiyhl z80)
