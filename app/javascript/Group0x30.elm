module Group0x30 exposing (..)

import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Z80Core exposing (Z80Core, imm16)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (mem)
import Z80Rom exposing (Z80ROM)


delta_dict_lite_30 : Dict Int (Z80ROM -> Z80Core -> Z80Delta)
delta_dict_lite_30 =
    Dict.fromList
        [ ( 0x3A, ld_a_indirect_nn ) -- need triple byte with env for this
        ]


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
