module Z80CoreWithClockTime exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime, reset_cpu_time)
import Z80Core exposing (Z80Core, set_iff)
import Z80Env exposing (z80_push, z80env_constructor)
import Z80Flags exposing (FlagRegisters)
import Z80Mem exposing (mem16)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (InterruptMode(..), MainRegisters)


type alias Z80CoreWithClockTime =
    { core : Z80Core
    , clockTime : CpuTimeCTime
    }


constructor : Z80CoreWithClockTime
constructor =
    let
        main =
            Z80Types.MainWithIndexRegisters 0 0 0 0 0 0 0

        flags =
            FlagRegisters 0 0 0 0 0

        --alt_flags =
        --    FlagRegisters 0 0 0 0 0
        interrupts =
            Z80Types.InterruptRegisters IM0 False 0 0 0

        time =
            reset_cpu_time
    in
    --Z80 z80env_constructor 0 main main_flags alternate alt_flags 0 interrupts
    Z80CoreWithClockTime (Z80Core z80env_constructor 0 main flags interrupts) time


type alias Z80 =
    { coreWithClock : Z80CoreWithClockTime
    , alt_main : MainRegisters
    , alt_flags : FlagRegisters
    }


add_cpu_time : Int -> Z80CoreWithClockTime -> Z80CoreWithClockTime
add_cpu_time value z80 =
    let
        env =
            --z80.env |> addCpuTimeEnv value
            z80.clockTime |> addCpuTimeTime value
    in
    { z80 | clockTime = env }


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
        z80Clock =
            full_z80.coreWithClock

        z80_core =
            z80Clock.core

        ints =
            z80_core.interrupts

        main =
            z80_core.main
    in
    if Bitwise.and ints.iff 1 == 0 then
        full_z80

    else
        let
            --z81 = debug_log "interrupt" "keyboard scan" z80
            z80_1 =
                { z80_core | interrupts = { ints | halted = False, iff = 0 } }

            pushed =
                z80_1.env |> z80_push z80_1.pc z80Clock.clockTime

            new_core =
                { z80_1 | env = pushed }

            newClock =
                { z80Clock | core = new_core, clockTime = z80Clock.clockTime |> addCpuTimeTime 6 }

            new_z80 =
                { full_z80 | coreWithClock = newClock }
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
                        new_core.env |> mem16 addr rom48k newClock.clockTime

                    core_1 =
                        { new_core | pc = env_and_pc.value16 }

                    newClock1 =
                        { newClock | core = core_1, clockTime = env_and_pc.time |> addCpuTimeTime 6 }
                in
                { new_z80 | coreWithClock = newClock1 }



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
        clock =
            z80.coreWithClock

        core =
            clock.core
    in
    { z80 | coreWithClock = { clock | core = { core | pc = Bitwise.and pc 0xFFFF } } }


get_ei : Z80 -> Bool
get_ei z80 =
    --	boolean ei() {return (IFF&1)!=0;}
    Bitwise.and z80.coreWithClock.core.interrupts.iff 1 /= 0


di_0xF3 : Z80 -> Z80
di_0xF3 full_z80 =
    -- case 0xF3: IFF=0; break;
    let
        clock =
            full_z80.coreWithClock

        z80_core =
            clock.core

        ints =
            z80_core |> set_iff 0

        new_core =
            { z80_core | interrupts = ints }
    in
    { full_z80 | coreWithClock = { clock | core = new_core } }


ei_0xFB : Z80 -> Z80
ei_0xFB full_z80 =
    --    -- case 0xFB: IFF=3; break;
    let
        clock =
            full_z80.coreWithClock

        z80 =
            clock.core

        ints =
            z80 |> set_iff 3

        new_core =
            { z80 | interrupts = ints }
    in
    { full_z80 | coreWithClock = { clock | core = new_core } }
