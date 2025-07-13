module Group0x90 exposing (..)

import Dict exposing (Dict)
import Z80Core exposing (Z80Core, hl_deref_with_z80_ixiy)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (sbc, z80_sub)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, get_h_ixiy, get_l_ixiy)


miniDict90 : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
miniDict90 =
    Dict.fromList
        [ ( 0x96, sub_indirect_hl )
        , ( 0x9E, sbc_indirect_hl )
        ]


sub_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
sub_indirect_hl ixiyhl rom48k z80 =
    -- case 0x96: sub(env.mem(HL)); time+=3; break;
    -- case 0x96: sub(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        --env_1 = z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (z80_sub value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> z80_sub value.value) value.pc value.time


sbc_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
sbc_indirect_hl ixiyhl rom48k z80 =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    -- case 0x9E: sbc(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        --env_1 = z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (sbc value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> sbc value.value) value.pc value.time
