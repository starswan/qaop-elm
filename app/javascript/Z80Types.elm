module Z80Types exposing (..)

import Bitwise exposing (shiftRightBy)
import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, CpuTimePcAnd16BitValue, CpuTimePcAndValue, addCpuTimeTime)
import Utils exposing (char, shiftLeftBy8, shiftRightBy8, wordPlusOffset)
import Z80Env exposing (Z80Env, Z80EnvWithPC, addCpuTimeEnv, c_TIME_LIMIT, mem, mem16, setMem, z80_push)
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


type alias Z80 =
    { env : Z80Env
    , pc : Int
    , main : MainWithIndexRegisters
    , flags : FlagRegisters
    , alt_main : MainRegisters
    , alt_flags : FlagRegisters
    , r : Int
    , interrupts : InterruptRegisters

    --, time_limit : Int
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



-- would need the side-effect of mem call as well
--imm8_discard: Z80 -> Z80
--imm8_discard z80 =
--    z80 |> inc_pc |> add_cpu_time 3
--	private int imm16()
--	{
--		int v = env.mem16(PC);
--		PC = (char)(PC+2);
--		time += 6;
--		return v;
--	}


imm16 : Z80ROM -> Z80 -> CpuTimePcAnd16BitValue
imm16 rom48k z80 =
    let
        v =
            z80.env |> mem16 z80.pc rom48k

        pc =
            Bitwise.and (z80.pc + 2) 0xFFFF

        env =
            v.time |> addCpuTimeTime 6
    in
    CpuTimePcAnd16BitValue env pc v.value16



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


a_with_z80 : Z80 -> CpuTimePcAndValue
a_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.flags.a


add_cpu_time : Int -> Z80 -> Z80
add_cpu_time value z80 =
    let
        env =
            z80.env |> addCpuTimeEnv value
    in
    { z80 | env = env }


get_ixiy_xy : IXIY -> MainWithIndexRegisters -> Int
get_ixiy_xy ixiy z80_main =
    case ixiy of
        IXIY_IX ->
            z80_main.ix

        IXIY_IY ->
            z80_main.iy


hl_deref_with_z80_ixiy : IXIY -> Z80ROM -> Z80 -> CpuTimePcAndValue
hl_deref_with_z80_ixiy ixiyhl rom48k z80 =
    let
        a =
            z80 |> env_mem_hl_ixiy ixiyhl rom48k

        new_b =
            mem a.value16 z80.env.time rom48k z80.env.ram
    in
    CpuTimePcAndValue new_b.time a.pc new_b.value


inc_pc : Z80 -> Int
inc_pc z80 =
    Bitwise.and (z80.pc + 1) 0xFFFF


inc_pc2 : Z80 -> Int
inc_pc2 z80 =
    Bitwise.and (z80.pc + 2) 0xFFFF


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
--getd_value: Int -> Z80 -> CpuTimePcAndValue
--getd_value xy z80 =
--   let
--      d = z80.env |> mem z80.pc
--   in
--      CpuTimePcAndValue (d.time |> add_cpu_time_time 8) (char (z80.pc + 1)) (char (xy + byte d.value))


env_mem_hl_ixiy : IXIY -> Z80ROM -> Z80 -> CpuTimePcAnd16BitValue
env_mem_hl_ixiy ixiyhl rom48k z80 =
    case ixiyhl of
        IXIY_IX ->
            let
                dval =
                    mem z80.pc z80.env.time rom48k z80.env.ram
            in
            CpuTimePcAnd16BitValue dval.time (char (z80.pc + 1)) (z80.main.ix |> wordPlusOffset dval.value)

        IXIY_IY ->
            let
                dval =
                    mem z80.pc z80.env.time rom48k z80.env.ram
            in
            CpuTimePcAnd16BitValue dval.time (char (z80.pc + 1)) (z80.main.iy |> wordPlusOffset dval.value)


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



--	void iff(int v) {IFF = v;}


set_iff : Int -> Z80 -> InterruptRegisters
set_iff value z80 =
    let
        --y = debug_log "set_iff" value Nothing
        interrupts =
            z80.interrupts
    in
    { interrupts | iff = value }



--	int af() {return A<<8 | flags();}
--get_af_z80 : Z80 -> Int
--get_af_z80 z80 =
--    z80.flags |> get_af


set408bit : Int -> Int -> IXIYHL -> Z80 -> Z80
set408bit c value ixiyhl z80 =
    case Bitwise.and c 0x07 of
        0 ->
            let
                z80_main =
                    z80.main
            in
            { z80 | main = { z80_main | b = value } }

        1 ->
            let
                z80_main =
                    z80.main
            in
            { z80 | main = { z80_main | c = value } }

        2 ->
            let
                z80_main =
                    z80.main
            in
            { z80 | main = { z80_main | d = value } }

        3 ->
            let
                z80_main =
                    z80.main
            in
            { z80 | main = { z80_main | e = value } }

        4 ->
            let
                main =
                    z80.main |> set_h value ixiyhl
            in
            { z80 | main = main }

        5 ->
            let
                main =
                    z80.main |> set_l value ixiyhl
            in
            { z80 | main = main }

        6 ->
            { z80 | env = setMem z80.main.hl value z80.env }

        _ ->
            let
                z80_flags =
                    z80.flags
            in
            { z80 | flags = { z80_flags | a = value } }


im0 : Int -> Z80 -> Z80
im0 bus z80 =
    if Bitwise.and bus 0x38 == 0xFF then
        let
            new_pc =
                bus - 199
        in
        z80 |> set_pc new_pc

    else
        z80


interrupt : Int -> Z80ROM -> Z80 -> Z80
interrupt bus rom48k z80 =
    let
        ints =
            z80.interrupts
    in
    if Bitwise.and ints.iff 1 == 0 then
        z80

    else
        let
            --z81 = debug_log "interrupt" "keyboard scan" z80
            new_ints =
                { ints | iff = 0, halted = False }

            z80_1 =
                { z80 | interrupts = new_ints }

            pushed =
                z80_1.env |> z80_push z80_1.pc

            new_z80 =
                { z80_1 | env = pushed |> addCpuTimeEnv 6 }
        in
        case ints.iM of
            0 ->
                new_z80 |> im0 bus

            1 ->
                new_z80 |> im0 bus

            2 ->
                { new_z80 | pc = 0x38 }

            3 ->
                let
                    new_ir =
                        Bitwise.and ints.ir 0xFF00

                    addr =
                        Bitwise.or new_ir bus

                    env_and_pc =
                        z80.env |> mem16 addr rom48k

                    env =
                        z80.env
                in
                { new_z80 | env = { env | time = env_and_pc.time } |> addCpuTimeEnv 6, pc = env_and_pc.value16 }

            _ ->
                new_z80



--	void pc(int v) {PC = v;}


set_pc : Int -> Z80 -> Z80
set_pc pc z80 =
    let
        -- ignore common routines and LDIR/LDDR and friends (jump back 2)
        --y = if Dict.member pc Z80Rom.c_COMMON_NAMES || pc == z80.pc - 2 then
        --      Nothing
        --    else
        --      let
        --        sub_name = pc |> subName
        --      in
        --        if sub_name|> String.startsWith "CHAN-OPEN" then
        --          debug_log sub_name (z80.flags.a |> toHexString2) Nothing
        --        else if sub_name |> String.startsWith "PRINT-OUT " then
        --           debug_log sub_name (z80.flags.a |> toHexString2) Nothing
        --        else if sub_name |> String.startsWith "PO-CHAR " then
        --           debug_log sub_name ("DE " ++ (z80 |> get_de |> toHexString) ++
        --                                        " HL " ++ (z80.main.hl |> toHexString) ++
        --                                        " BC " ++ (z80 |> get_bc |> toHexString) ++
        --                                        " A " ++ (z80.flags.a |> toHexString2)) Nothing
        --        else if sub_name |> String.startsWith "PR-ALL-3 " then
        --           debug_log sub_name ("DE " ++ (z80 |> get_de |> toHexString) ++
        --                                        " HL " ++ (z80.main.hl |> toHexString) ++
        --                                        " B " ++ (z80.main.b |> toHexString2) ++
        --                                        " C " ++ (z80.main.c |> toHexString2)) Nothing
        --      else
        --          debug_log "set_pc" ("from " ++ (z80.pc |> toHexString) ++
        --                           " to " ++ (pc |> subName) ++
        --                           " (sp " ++ (z80.sp |> toHexString) ++ ")") Nothing
        z80_1 =
            { z80 | pc = Bitwise.and pc 0xFFFF }
    in
    z80_1



--	boolean ei() {return (IFF&1)!=0;}


get_ei : Z80 -> Bool
get_ei z80 =
    Bitwise.and z80.interrupts.iff 1 /= 0


z80_halt : Z80 -> Z80
z80_halt z80 =
    let
        interrupts =
            z80.interrupts

        --n = shiftRightBy 2 (z80.time_limit - z80.env.time.cpu_time + 3)
        n =
            shiftRightBy 2 (c_TIME_LIMIT - z80.env.time.cpu_time + 3)

        z80_1 =
            if n > 0 then
                -- turns out env.halt(n, r) just returns n...?
                { z80 | r = z80.r + n } |> add_cpu_time (4 * n)

            else
                z80
    in
    { z80_1 | interrupts = { interrupts | halted = True } }
