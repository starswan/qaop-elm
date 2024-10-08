module Group0x10 exposing (..)

import Bitwise
import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Utils exposing (byte, shiftLeftBy8, shiftRightBy8)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (addCpuTimeEnv, mem)
import Z80Flags exposing (add16, dec, inc, rot)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL, Z80, add_cpu_time, get_de, get_xy, imm16, imm8, set_de_main, set_xy)

delta_dict_10 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_10 =
    Dict.fromList
        [
          (0x19, execute_0x19)
        ]


delta_dict_lite_10 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_10 =
    Dict.fromList
        [
          (0x10, execute_0x10),
          (0x11, execute_0x11),
          (0x12, execute_0x12),
          (0x13, execute_0x13),
          (0x14, execute_0x14),
          (0x15, execute_0x15),
          (0x16, execute_0x16),
          (0x17, execute_0x17),
          (0x18, execute_0x18),
          (0x1A, execute_0x1A),
          (0x1B, execute_0x1B),
          (0x1C, execute_0x1C),
          (0x1D, execute_0x1D),
          (0x1E, execute_0x1E),
          (0x1F, execute_0x1F)
        ]


execute_0x10 : Z80ROM -> Z80 -> Z80Delta
execute_0x10 rom48k z80 =
    --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
    --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
    --PC=(char)v;} break;
    let
        z80_main =
            z80.main

        z80_1 =
            z80 |> add_cpu_time 1

        v =
            z80_1.pc

        mem_value =
            z80_1.env |> mem v rom48k

        d =
            byte mem_value.value

        env_0 =
            z80.env

        z80_2 =
            { z80_1 | env = { env_0 | time = mem_value.time |> addCpuTimeTime 3 } }

        b =
            Bitwise.and (z80_2.main.b - 1) 0xFF

        ( z80_3, v3 ) =
            if b /= 0 then
                ( z80_2.env.time |> addCpuTimeTime 5, v + 1 + d )

            else
                ( z80_2.env.time, v + 1 )
    in
    --{ z80_3 | main = { z80_main | b = b } } |> set_pc v3
    MainRegsWithPcAndCpuTime { z80_main | b = b } v3 z80_3


execute_0x11 : Z80ROM -> Z80 -> Z80Delta
execute_0x11 rom48k z80 =
    --case 0x11: v=imm16(); D=v>>>8; E=v&0xFF; break;
    let
        v =
           z80 |> imm16 rom48k

        main_regs =
            z80.main |> set_de_main v.value
    in
    MainRegsWithPcAndCpuTime main_regs v.pc v.time


execute_0x12 : Z80ROM -> Z80 -> Z80Delta
execute_0x12 rom48k z80 =
    -- case 0x12: MP=(v=D<<8|E)+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    let
        addr =
            shiftLeftBy8 z80.main.d + z80.main.e
    in
    --z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3 |> OnlyEnv
    SetMem8WithTime addr z80.flags.a 3


execute_0x13 : Z80ROM -> Z80 -> Z80Delta
execute_0x13 rom48k z80 =
    -- case 0x13: if(++E==256) {D=D+1&0xFF;E=0;} time+=2; break;
    let
        z80_main =
            z80.main

        tmp_e =
            z80_main.e + 1

        ( reg_d, reg_e ) =
            if tmp_e == 256 then
                ( Bitwise.and (z80_main.d + 1) 0xFF, 0 )

            else
                ( z80_main.d, tmp_e )

        env_1 =
            z80.env |> addCpuTimeEnv 2

        main_1 =
            { z80_main | d = reg_d, e = reg_e }
    in
    --{ z80 | env = env_1, main = main_1 }
    MainRegsWithEnv main_1 env_1


execute_0x14 : Z80ROM -> Z80 -> Z80Delta
execute_0x14 rom48k z80 =
    -- case 0x14: D=inc(D); break;
    let
        new_d =
            inc z80.main.d z80.flags

        z80_main =
            z80.main

        main_1 =
            { z80_main | d = new_d.value }
    in
    --{ z80 | flags = new_d.flags, main = { z80_main | d = new_d.value } }
    FlagsWithMain new_d.flags main_1


execute_0x15 : Z80ROM -> Z80 -> Z80Delta
execute_0x15 rom48k z80 =
    -- case 0x15: D=dec(D); break;
    let
        new_d =
            dec z80.main.d z80.flags

        z80_main =
            z80.main

        main_1 =
            { z80_main | d = new_d.value }
    in
    --{ z80 | flags = new_d.flags, main = main_1 }
    FlagsWithMain new_d.flags main_1


execute_0x16 : Z80ROM -> Z80 -> Z80Delta
execute_0x16 rom48k z80 =
    -- case 0x16: D=imm8(); break;
    let
        z80_main =
            z80.main

        new_d =
            z80 |> imm8 rom48k

        main_1 =
            { z80_main | d = new_d.value }
    in
    --{ z80 | pc = new_d.pc, env = new_d.env, main = main_1 }
    MainRegsWithPcAndCpuTime main_1 new_d.pc new_d.time


execute_0x17 : Z80ROM -> Z80 -> Z80Delta
execute_0x17 rom48k z80 =
    -- case 0x17: rot(A<<1|Ff>>>8&1); break;
    -- { z80 | flags = z80.flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80.flags.a)
    --                                                                           (Bitwise.and (shiftRightBy8 z80.flags.ff) 1)) }
    let
        flags =
            z80.flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80.flags.a) (Bitwise.and (shiftRightBy8 z80.flags.ff) 1))
    in
    FlagRegs flags


execute_0x18 : Z80ROM -> Z80 -> Z80Delta
execute_0x18 rom48k z80 =
    -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
    -- This is just an inlined jr() call
    let
        mem_value =
            z80.env |> mem z80.pc rom48k

        pc_val =
            z80.pc + 1 + byte mem_value.value

        --pc_val = ProgramCounter dest
        --x = if (dest |> subName |> (String.startsWith "CALL-SUB")) then
        --      -- HL still need to be in-directed, so not a subroutine address yet
        --      let
        --         called = z80.env |> mem16 z80.main.hl
        --      in
        --         debug_log "CALL-SUB" ("from " ++ (z80.pc |> toHexString) ++ " to " ++ (called.value |> subName)) Nothing
        --    else
        --      if Dict.member dest Z80Rom.c_COMMON_NAMES then
        --         Nothing
        --      else
        --         debug_log "jr" (dest |> subName) Nothing
    in
    --z80 |> set_pc dest |> add_cpu_time 8
    --PcAndCpuTime pc_val 8
    CpuTimeWithPc (mem_value.time |> addCpuTimeTime 8) pc_val


execute_0x19 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x19 ixiyhl rom48k z80 =
    -- case 0x19: HL=add16(HL,D<<8|E); break;
    -- case 0x19: xy=add16(xy,D<<8|E); break;
    let
        xy =
            get_xy ixiyhl z80.main

        new_xy =
            add16 xy (get_de z80.main) z80.flags

        new_z80 =
            set_xy new_xy.value ixiyhl z80.main
    in
    --{ z80 | main = new_z80, flags = new_xy.flags} |> add_cpu_time new_xy.time
    FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time


execute_0x1A : Z80ROM -> Z80 -> Z80Delta
execute_0x1A rom48k z80 =
    -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
    let
        z80_main =
            z80.main

        addr =
            Bitwise.or (shiftLeftBy8 z80_main.d) z80_main.e

        new_a =
            z80.env |> mem addr rom48k

        main_flags =
            z80.flags

        new_flags =
            { main_flags | a = new_a.value }

        env_1 =
            new_a.time |> addCpuTimeTime 3
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    CpuTimeWithFlags env_1 new_flags


execute_0x1B : Z80ROM -> Z80 -> Z80Delta
execute_0x1B rom48k z80 =
    -- case 0x1B: if(--E<0) D=D-1&(E=0xFF); time+=2; break;
    let
        z80_main =
            z80.main

        tmp_e =
            z80_main.e - 1

        ( reg_d, reg_e ) =
            if tmp_e < 0 then
                ( Bitwise.and (z80_main.d - 1) 0xFF, 0xFF )

            else
                ( z80_main.d, tmp_e )

        main_1 =
            { z80_main | d = reg_d, e = reg_e }
    in
    --{ z80 | main = main_1 } |> add_cpu_time 2
    MainRegsAndCpuTime main_1 2


execute_0x1C : Z80ROM -> Z80 -> Z80Delta
execute_0x1C rom48k z80 =
    -- case 0x1C: E=inc(E); break;
    let
        z80_main =
            z80.main

        new_e =
            inc z80.main.e z80.flags

        main_1 =
            { z80_main | e = new_e.value }
    in
    --{ z80 | flags = new_e.flags, main = { z80_main | e = new_e.value } }
    FlagsWithMain new_e.flags main_1


execute_0x1D : Z80ROM -> Z80 -> Z80Delta
execute_0x1D rom48k z80 =
    -- case 0x1D: E=dec(E); break;
    let
        z80_main =
            z80.main

        new_e =
            dec z80.main.c z80.flags

        main_1 =
            { z80_main | e = new_e.value }
    in
    --{ z80 | flags = new_e.flags, main = main_1 }
    FlagsWithMain new_e.flags main_1


execute_0x1E : Z80ROM -> Z80 -> Z80Delta
execute_0x1E rom48k z80 =
    -- case 0x1E: E=imm8(); break;
    let
        z80_main =
            z80.main

        new_e =
            z80 |> imm8 rom48k

        main_1 =
            { z80_main | e = new_e.value }
    in
    --{ z80 | env = new_e.env, pc = new_e.pc, main = main_1 }
    MainRegsWithPcAndCpuTime main_1 new_e.pc new_e.time


execute_0x1F : Z80ROM -> Z80 -> Z80Delta
execute_0x1F rom48k z80 =
    -- case 0x1F: rot((A*0x201|Ff&0x100)>>>1); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80.flags.a * 0x201)
    --                                                                           (Bitwise.and z80.flags.ff 0x100))) }
    let
        flags =
            z80.flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80.flags.a * 0x0201) (Bitwise.and z80.flags.ff 0x0100)))
    in
    FlagRegs flags
