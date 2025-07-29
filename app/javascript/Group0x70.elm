module Group0x70 exposing (..)

import CpuTimeCTime
import Dict exposing (Dict)
import Z80Core exposing (Z80Core, env_mem_hl_ixiy, hl_deref_with_z80_ixiy)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY)


miniDict70 : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
miniDict70 =
    Dict.fromList
        [ ( 0x77, ld_indirect_hl_a )
        ]



--execute_0x7077_ixiy : IXIY -> Z80ROM -> Z80Core -> Int -> Z80Delta
--execute_0x7077_ixiy ixiyhl rom48k z80 value =
--    -- case 0x70: env.mem(HL,B); time+=3; break;
--    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
--    let
--        mem_target =
--            z80 |> env_mem_hl_ixiy ixiyhl rom48k
--
--        --env_1 =
--        --    z80.env
--        --new_env =
--        --    { env_1 | time = mem_target.time }
--        --        |> setMem mem_target.value value
--        --        |> addCpuTimeEnv 3
--    in
--    --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
--    SetMem8WithCpuTimeIncrementAndPc mem_target.value16 value mem_target.time 3 mem_target.pc


ld_indirect_hl_a : IXIY -> Z80ROM -> Z80Core -> Z80Delta
ld_indirect_hl_a ixiyhl rom48k z80 =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    let
        value =
            z80.flags.a

        mem_target =
            z80 |> env_mem_hl_ixiy ixiyhl rom48k
    in
    --execute_0x7077_ixiy ixiyhl rom48k z80 z80.flags.a
    SetMem8WithCpuTimeIncrementAndPc mem_target.value16 value mem_target.time 3 mem_target.pc



--ld_a_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
--ld_a_indirect_hl ixiyhl rom48k z80 =
--    -- case 0x7E: A=env.mem(HL); time+=3; break;
--    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
--    let
--        value =
--            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k
--
--        --env_1 = z80.env
--        flags =
--            z80.flags
--    in
--    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
--    FlagsWithPcAndTime { flags | a = value.value } value.pc value.time
