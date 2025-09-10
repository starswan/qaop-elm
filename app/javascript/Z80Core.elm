module Z80Core exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimePcAnd16BitValue, CpuTimePcAndValue, addCpuTimeTime)
import Utils exposing (shiftLeftBy8)
import Z80Env exposing (Z80Env, addCpuTimeEnv, mem16, setMem, z80_push)
import Z80Flags exposing (FlagRegisters)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (InterruptMode(..), InterruptRegisters, MainRegisters, MainWithIndexRegisters)


type alias Z80Core =
    { env : Z80Env
    , pc : Int
    , main : MainWithIndexRegisters
    , flags : FlagRegisters
    , interrupts : InterruptRegisters
    }


type alias Z80 =
    { core : Z80Core
    , alt_main : MainRegisters
    , alt_flags : FlagRegisters
    }



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


imm16 : Z80ROM -> Z80Core -> CpuTimePcAnd16BitValue
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


add_cpu_time : Int -> Z80Core -> Z80Core
add_cpu_time value z80 =
    let
        env =
            z80.env |> addCpuTimeEnv value
    in
    { z80 | env = env }


set_iff : Int -> Z80Core -> InterruptRegisters
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


set408bitHL : Int -> Int -> ( MainWithIndexRegisters, FlagRegisters, Z80Env ) -> ( MainWithIndexRegisters, FlagRegisters, Z80Env )
set408bitHL c value ( z80_main, z80_flags, z80_env ) =
    case Bitwise.and c 0x07 of
        0 ->
            ( { z80_main | b = value }, z80_flags, z80_env )

        1 ->
            ( { z80_main | c = value }, z80_flags, z80_env )

        2 ->
            ( { z80_main | d = value }, z80_flags, z80_env )

        3 ->
            ( { z80_main | e = value }, z80_flags, z80_env )

        4 ->
            ( { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 value) }, z80_flags, z80_env )

        5 ->
            ( { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) value }, z80_flags, z80_env )

        6 ->
            ( z80_main, z80_flags, z80_env |> setMem z80_main.hl value )

        _ ->
            ( z80_main, { z80_flags | a = value }, z80_env )


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
interrupt bus rom48k full_z80 =
    let
        z80 =
            full_z80.core

        ints =
            z80.interrupts

        main =
            z80.main
    in
    if Bitwise.and ints.iff 1 == 0 then
        full_z80

    else
        let
            --z81 = debug_log "interrupt" "keyboard scan" z80
            --new_ints =
            --    { ints | iff = 0, halted = False }
            z80_1 =
                { z80 | interrupts = { ints | halted = False, iff = 0 } }

            pushed =
                z80_1.env |> z80_push z80_1.pc

            new_core =
                { z80_1 | env = pushed |> addCpuTimeEnv 6 }

            new_z80 =
                { full_z80 | core = new_core }
        in
        case ints.iM of
            IM0 ->
                new_z80 |> im0 bus

            --1 ->
            --    new_z80 |> im0 bus
            IM1 ->
                new_z80 |> set_pc 0x38

            IM2 ->
                let
                    new_ir =
                        Bitwise.and ints.ir 0xFF00

                    addr =
                        Bitwise.or new_ir bus

                    env_and_pc =
                        z80.env |> mem16 addr rom48k

                    env =
                        z80.env

                    core_1 =
                        { new_core | env = { env | time = env_and_pc.time } |> addCpuTimeEnv 6, pc = env_and_pc.value16 }
                in
                { new_z80 | core = core_1 }



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



--z80_halt : Z80 -> Z80
--z80_halt z80 =
--    let
--        z80_core =
--            z80.core
--
--        interrupts =
--            z80_core.interrupts
--
--        --n = shiftRightBy 2 (z80.time_limit - z80.env.time.cpu_time + 3)
--        n =
--            shiftRightBy 2 (c_TIME_LIMIT - z80_core.env.time.cpu_time + 3)
--
--        z80_1 =
--            if n > 0 then
--                -- turns out env.halt(n, r) just returns n...?
--                { z80_core | interrupts = { interrupts | r = interrupts.r + n } } |> add_cpu_time (4 * n)
--
--            else
--                z80_core
--
--        core_2 =
--            { z80_1 | interrupts = { interrupts | halted = True } }
--    in
--    { z80 | core = core_2 }
