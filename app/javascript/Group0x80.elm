module Group0x80 exposing (..)

import Dict exposing (Dict)
import Z80Core exposing (Z80Core, hl_deref_with_z80_ixiy)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (adc, z80_add)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, get_h_ixiy, get_l_ixiy)


miniDict80 : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
miniDict80 =
    Dict.fromList
        [ ( 0x86, add_a_indirect_hl )
        , ( 0x8E, adc_a_indirect_hl )
        ]


add_a_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
add_a_indirect_hl ixiyhl rom48k z80 =
    -- case 0x86: add(env.mem(HL)); time+=3; break;
    -- case 0x86: add(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        flags_one =
            z80_add value.value z80.flags
    in
    FlagsWithPcAndTime flags_one value.pc value.time


adc_a_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
adc_a_indirect_hl ixiyhl rom48k z80 =
    -- case 0x8E: adc(env.mem(HL)); time+=3; break;
    -- case 0x8E: adc(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        --env_1 = z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (adc value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> adc value.value) value.pc value.time
