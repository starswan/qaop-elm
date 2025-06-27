module Group0x30 exposing (..)

import Bitwise
import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Utils exposing (byte, char)
import Z80Core exposing (Z80Core, imm16)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (addCpuTimeEnv, mem, setMem)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL(..), get_xy_ixiy)


miniDict30 : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
miniDict30 =
    Dict.fromList
        [ --( 0x34, inc_indirect_hl )
          --, ( 0x35, dec_indirect_hl )
          ( 0x36, ld_indirect_hl_n )
        ]


delta_dict_lite_30 : Dict Int (Z80ROM -> Z80Core -> Z80Delta)
delta_dict_lite_30 =
    Dict.fromList
        [ ( 0x3A, ld_a_indirect_nn ) -- need triple byte with env for this
        ]


ld_indirect_hl_n : IXIY -> Z80ROM -> Z80Core -> Z80Delta
ld_indirect_hl_n ixiyhl rom48k z80 =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    --	v=env.mem((char)(PC+1)); time+=5;
    --	env.mem(a,v); PC=(char)(PC+2); time+=3;} break;
    let
        xy =
            get_xy_ixiy ixiyhl z80.main

        mempc =
            z80.env |> mem z80.pc z80.env.time rom48k

        env_1 =
            z80.env

        a =
            char (xy + byte mempc.value)

        v =
            env_1 |> mem (char (z80.pc + 1)) (mempc.time |> addCpuTimeTime 3) rom48k

        z80_2 =
            { env_1 | time = v.time |> addCpuTimeTime 5 }

        x =
            z80_2 |> setMem a v.value

        new_pc =
            Bitwise.and (z80.pc + 2) 0xFFFF
    in
    --{ z80_2 | env = x, pc = new_pc } |> add_cpu_time 3
    EnvWithPc (x |> addCpuTimeEnv 3) new_pc


ld_a_indirect_nn : Z80ROM -> Z80Core -> Z80Delta
ld_a_indirect_nn rom48k z80 =
    -- case 0x3A: MP=(v=imm16())+1; A=env.mem(v); time+=3; break;
    let
        z80_flags =
            z80.flags

        v =
            z80 |> imm16 rom48k

        mem_value =
            z80.env |> mem v.value16 z80.env.time rom48k
    in
    CpuTimeWithFlagsAndPc (mem_value.time |> addCpuTimeTime 3) { z80_flags | a = mem_value.value } v.pc
