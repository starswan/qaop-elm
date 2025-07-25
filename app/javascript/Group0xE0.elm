module Group0xE0 exposing (..)

import Bitwise
import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import GroupED exposing (group_ed)
import Utils exposing (shiftLeftBy8)
import Z80Core exposing (Z80Core)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (addCpuTimeEnv, out, z80_in, z80_pop)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL(..), get_xy, get_xy_ixiy, imm8)


miniDictE0 : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
miniDictE0 =
    Dict.fromList
        [ ( 0xE1, pop_hl )
        , ( 0xE5, push_hl )
        , ( 0xE9, jp_hl )
        ]


delta_dict_lite_E0 : Dict Int (Z80ROM -> Z80Core -> Z80Delta)
delta_dict_lite_E0 =
    Dict.fromList
        [ ( 0xD3, execute_0xD3 )
        , ( 0xDB, execute_0xDB )
        , ( 0xED, group_ed )
        ]


pop_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
pop_hl ixiyhl rom48k z80 =
    -- case 0xE1: HL=pop(); break;
    -- case 0xE1: xy=pop(); break;
    let
        hl =
            z80.env |> z80_pop rom48k

        --env = z80.env
        --z80_1 = { z80 | env = { env | time = hl.time, sp = hl.sp } }
        main =
            z80.main
    in
    case ixiyhl of
        IXIY_IX ->
            MainRegsWithSpPcAndTime { main | ix = hl.value16 } hl.sp z80.pc hl.time

        IXIY_IY ->
            MainRegsWithSpPcAndTime { main | iy = hl.value16 } hl.sp z80.pc hl.time


push_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
push_hl ixiyhl _ z80 =
    -- case 0xE5: push(HL); break;
    -- case 0xE5: push(xy); break;
    let
        toBePushed =
            z80.main |> get_xy_ixiy ixiyhl

        --pushed =
        --    z80.env |> z80_push toBePushed
    in
    --{ z80 | env = pushed }
    --EnvWithPc pushed z80.pc
    PushWithPc toBePushed z80.pc


jp_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
jp_hl ixiyhl _ z80 =
    -- case 0xE9: PC=HL; break;
    -- case 0xE9: PC=xy; break;
    let
        xy =
            z80.main |> get_xy_ixiy ixiyhl

        --a = if Dict.member xy Z80Rom.c_COMMON_NAMES then
        --      Nothing
        --    else
        --      debug_log ("JP (" ++ (toString ixiyhl) ++ ")") (xy |> subName) Nothing
    in
    --{ z80 | pc = xy }
    OnlyPc xy


execute_0xD3 : Z80ROM -> Z80Core -> Z80Delta
execute_0xD3 rom48k z80 =
    -- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
    let
        value =
            z80.env |> imm8 z80.pc z80.env.time rom48k

        env_1 =
            z80.env

        env_2 =
            { env_1 | time = value.time }

        v =
            Bitwise.or value.value (shiftLeftBy8 z80.flags.a)

        env =
            out v z80.flags.a env_2 |> addCpuTimeEnv 4
    in
    EnvWithPc env value.pc


execute_0xDB : Z80ROM -> Z80Core -> Z80Delta
execute_0xDB rom48k z80 =
    -- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
    let
        imm8val =
            z80.env |> imm8 z80.pc z80.env.time rom48k

        env_1 =
            z80.env

        z80_1 =
            { z80 | env = { env_1 | time = imm8val.time }, pc = imm8val.pc }

        v =
            Bitwise.or imm8val.value (shiftLeftBy8 z80_1.flags.a)

        a =
            z80_1.env |> z80_in v rom48k.keyboard

        flags =
            z80_1.flags

        new_flags =
            { flags | a = a.value }
    in
    --{ z80_1 | env = a.env, flags = { flags | a = a.value } }
    CpuTimeWithFlagsAndPc imm8val.time new_flags imm8val.pc
