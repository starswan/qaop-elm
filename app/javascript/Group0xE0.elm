module Group0xE0 exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimePcAndValue, addCpuTimeTime)
import Dict exposing (Dict)
import GroupED exposing (group_ed)
import Utils exposing (shiftLeftBy8)
import Z80Core exposing (Z80Core)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (Z80Env, mem, z80_in, z80_out)
import Z80Rom exposing (Z80ROM)


delta_dict_lite_E0 : Dict Int (Z80ROM -> Z80Core -> Z80Delta)
delta_dict_lite_E0 =
    Dict.fromList
        [ ( 0xDB, execute_0xDB )
        , ( 0xED, group_ed )
        ]


execute_0xDB : Z80ROM -> Z80Core -> Z80Delta
execute_0xDB rom48k z80 =
    -- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
    let
        imm8val =
            z80.env |> imm8 z80.pc z80.clockTime rom48k

        z80_1 =
            { z80 | pc = imm8val.pc }

        v =
            Bitwise.or imm8val.value (shiftLeftBy8 z80_1.flags.a)

        a =
            z80_1.env |> z80_in v rom48k.keyboard imm8val.time

        flags =
            z80_1.flags

        new_flags =
            { flags | a = a.value }
    in
    --{ z80_1 | env = a.env, flags = { flags | a = a.value } }
    CpuTimeWithFlagsAndPc imm8val.time new_flags imm8val.pc



--
--	private int imm8()
--	{
--		int v = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 3;
--		return v;
--	}


imm8 : Int -> CpuTimeCTime -> Z80ROM -> Z80Env -> CpuTimePcAndValue
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
