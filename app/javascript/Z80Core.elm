module Z80Core exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimePcAnd16BitValue, addCpuTimeTime)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (InterruptMode(..), InterruptRegisters, MainRegisters, MainWithIndexRegisters)


type alias Z80Core =
    { env : Z80Env
    , pc : Int
    , main : MainWithIndexRegisters
    , flags : FlagRegisters
    , interrupts : InterruptRegisters
    , clockTime : CpuTimeCTime
    }


type alias Z80 =
    { core : Z80Core
    , alt_main : MainRegisters
    , alt_flags : FlagRegisters
    }


type DirectionForLDIR
    = Forwards
    | Backwards



--	private int imm16()
--	{
--		int v = env.mem16(PC);
--		PC = (char)(PC+2);
--		time += 6;
--		return v;
--	}


add_cpu_time : Int -> Z80Core -> Z80Core
add_cpu_time value z80 =
    let
        env =
            --z80.env |> addCpuTimeEnv value
            z80.clockTime |> addCpuTimeTime value
    in
    { z80 | clockTime = env }


set_iff : Int -> Z80Core -> InterruptRegisters
set_iff value z80 =
    let
        --logging this can be noisy
        --interrupts =
        --    debugLog "set_iff" value z80.interrupts
        interrupts =
            z80.interrupts
    in
    { interrupts | iff = value }



--	int af() {return A<<8 | flags();}


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



--_ ->
--    new_z80
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
        core =
            z80.core

        z80_1 =
            { z80 | core = { core | pc = Bitwise.and pc 0xFFFF } }
    in
    z80_1


get_ei : Z80 -> Bool
get_ei z80 =
    --	boolean ei() {return (IFF&1)!=0;}
    Bitwise.and z80.core.interrupts.iff 1 /= 0


di_0xF3 : Z80 -> Z80
di_0xF3 full_z80 =
    -- case 0xF3: IFF=0; break;
    let
        z80_core =
            full_z80.core

        ints =
            z80_core |> set_iff 0

        new_core =
            { z80_core | interrupts = ints }
    in
    { full_z80 | core = new_core }


ei_0xFB : Z80 -> Z80
ei_0xFB full_z80 =
    --    -- case 0xFB: IFF=3; break;
    let
        z80 =
            full_z80.core

        ints =
            z80 |> set_iff 3

        new_core =
            { z80 | interrupts = ints }
    in
    { full_z80 | core = new_core }
