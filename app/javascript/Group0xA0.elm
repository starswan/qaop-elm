module Group0xA0 exposing (..)

import Dict exposing (Dict)
import Z80Core exposing (Z80Core, hl_deref_with_z80_ixiy)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (z80_and, z80_xor)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, get_h_ixiy, get_l_ixiy)


miniDictA0 : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
miniDictA0 =
    Dict.fromList
        [ ( 0xA6, and_indirect_hl )
        , ( 0xAE, xor_indirect_hl )
        ]


and_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
and_indirect_hl ixiyhl rom48k z80 =
    -- case 0xA6: and(env.mem(HL)); time+=3; break;
    -- case 0xA6: and(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k
    in
    FlagsWithPcAndTime (z80.flags |> z80_and value.value) value.pc value.time


xor_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
xor_indirect_hl ixiyhl rom48k z80 =
    -- case 0xAE: xor(env.mem(HL)); time+=3; break;
    -- case 0xAE: xor(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k
    in
    FlagsWithPcAndTime (z80.flags |> z80_xor value.value) value.pc value.time
