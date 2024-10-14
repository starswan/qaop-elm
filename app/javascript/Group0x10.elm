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
        [ ( 0x19, execute_0x19 )
        ]


delta_dict_lite_10 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_10 =
    Dict.fromList
        [ ( 0x11, execute_0x11 )
        , ( 0x12, execute_0x12 )
        , ( 0x1A, execute_0x1A )
        ]


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
            mem addr z80.env.time rom48k z80.env.ram

        main_flags =
            z80.flags

        new_flags =
            { main_flags | a = new_a.value }

        env_1 =
            new_a.time |> addCpuTimeTime 3
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    CpuTimeWithFlags env_1 new_flags


