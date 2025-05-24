module Z80Types exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, CpuTimePcAnd16BitValue, CpuTimePcAndValue, addCpuTimeTime)
import Utils exposing (shiftLeftBy8, shiftRightBy8)
import Z80Env exposing (Z80Env, Z80EnvWithPC, mem)
import Z80Flags exposing (FlagRegisters)
import Z80Ram exposing (Z80Ram)
import Z80Rom exposing (Z80ROM)


type alias MainRegisters =
    { b : Int
    , c : Int
    , d : Int
    , e : Int
    , hl : Int
    }


type alias MainWithIndexRegisters =
    { b : Int
    , c : Int
    , d : Int
    , e : Int
    , hl : Int
    , ix : Int
    , iy : Int
    }


type alias InterruptRegisters =
    { ir : Int
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


imm8 : Int -> CpuTimeCTime -> Z80ROM -> Z80Ram -> CpuTimePcAndValue
imm8 pc time rom48k ram =
    let
        v =
            mem pc time rom48k ram

        new_pc =
            Bitwise.and (pc + 1) 0xFFFF

        env_1 =
            v.time |> addCpuTimeTime 3
    in
    CpuTimePcAndValue env_1 new_pc v.value



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


get_ixiy_xy : IXIY -> MainWithIndexRegisters -> Int
get_ixiy_xy ixiy z80_main =
    case ixiy of
        IXIY_IX ->
            z80_main.ix

        IXIY_IY ->
            z80_main.iy


set_h : Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_h value ixiyhl z80 =
    let
        xy =
            get_xy ixiyhl z80
    in
    set_xy (Bitwise.or (Bitwise.and xy 0xFF) (shiftLeftBy8 value)) ixiyhl z80


set_h_ixiy : Int -> IXIY -> MainWithIndexRegisters -> MainWithIndexRegisters
set_h_ixiy value ixiyhl z80 =
    let
        xy =
            get_xy_ixiy ixiyhl z80
    in
    set_xy_ixiy (Bitwise.or (Bitwise.and xy 0xFF) (shiftLeftBy8 value)) ixiyhl z80


set_l : Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_l value ixiyhl z80 =
    let
        xy =
            get_xy ixiyhl z80
    in
    set_xy (Bitwise.or (Bitwise.and xy 0xFF00) value) ixiyhl z80


set_l_ixiy : Int -> IXIY -> MainWithIndexRegisters -> MainWithIndexRegisters
set_l_ixiy value ixiyhl z80 =
    let
        xy =
            get_xy_ixiy ixiyhl z80
    in
    set_xy_ixiy (Bitwise.or (Bitwise.and xy 0xFF00) value) ixiyhl z80


get_xy : IXIYHL -> MainWithIndexRegisters -> Int
get_xy ixiyhl z80_main =
    case ixiyhl of
        IX ->
            z80_main.ix

        IY ->
            z80_main.iy

        HL ->
            z80_main.hl


get_xy_ixiy : IXIY -> MainWithIndexRegisters -> Int
get_xy_ixiy ixiyhl z80_main =
    case ixiyhl of
        IXIY_IX ->
            z80_main.ix

        IXIY_IY ->
            z80_main.iy


set_xy : Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_xy value ixiyhl z80 =
    case ixiyhl of
        IX ->
            { z80 | ix = value }

        IY ->
            { z80 | iy = value }

        HL ->
            { z80 | hl = value }


set_xy_ixiy : Int -> IXIY -> MainWithIndexRegisters -> MainWithIndexRegisters
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


get_bc : MainWithIndexRegisters -> Int
get_bc z80_main =
    z80_main.b |> shiftLeftBy8 |> Bitwise.or z80_main.c


get_de : MainWithIndexRegisters -> Int
get_de z80 =
    z80.d |> shiftLeftBy8 |> Bitwise.or z80.e


set_bc_main : Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_bc_main v z80_main =
    { z80_main | b = shiftRightBy8 v, c = Bitwise.and v 0xFF }


set_de_main : Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_de_main v z80_main =
    { z80_main | d = shiftRightBy8 v, e = Bitwise.and v 0xFF }



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


get_h : IXIYHL -> MainWithIndexRegisters -> Int
get_h ixiyhl z80 =
    shiftRightBy8 (get_xy ixiyhl z80)


get_h_ixiy : IXIY -> MainWithIndexRegisters -> Int
get_h_ixiy ixiyhl z80 =
    shiftRightBy8 (get_xy_ixiy ixiyhl z80)


get_l : IXIYHL -> MainWithIndexRegisters -> Int
get_l ixiyhl z80 =
    Bitwise.and (get_xy ixiyhl z80) 0xFF


get_l_ixiy : IXIY -> MainWithIndexRegisters -> Int
get_l_ixiy ixiyhl z80 =
    Bitwise.and (get_xy_ixiy ixiyhl z80) 0xFF
