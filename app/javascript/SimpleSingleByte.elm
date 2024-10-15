module SimpleSingleByte exposing (..)

import Bitwise
import Dict exposing (Dict)
import RegisterChange exposing (RegisterChange(..))
import Utils exposing (shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (Z80Change(..))
import Z80ChangeData exposing (RegisterChangeData, Z80ChangeData)
import Z80Flags exposing (FlagRegisters, adc, add16, dec, inc, z80_add, z80_sub)
import Z80Types exposing (IXIYHL(..), MainRegisters, MainWithIndexRegisters, Z80, get_bc)


singleByteMainAndFlagRegisters : Dict Int (MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData)
singleByteMainAndFlagRegisters =
    Dict.fromList
        [ ( 0x04, inc_b )
        , ( 0x05, dec_b )
        , ( 0x09, add_hl_bc )
        , ( 0x0C, inc_c )
        , ( 0x0D, dec_c )
        , ( 0x14, inc_d )
        , ( 0x15, dec_d )
        , ( 0x1C, inc_e )
        , ( 0x1D, dec_e )
        , ( 0x24, inc_h )
        , ( 0x25, dec_h )
        , ( 0x29, add_hl_hl )
        , ( 0x2C, inc_l )
        , ( 0x2D, dec_l )
        , ( 0x80, add_a_b )
        , ( 0x81, add_a_c )
        , ( 0x82, add_a_d )
        , ( 0x83, add_a_e )
        , ( 0x84, add_a_h )
        , ( 0x85, add_a_l )
        , ( 0x88, adc_a_b )
        , ( 0x89, adc_a_c )
        , ( 0x8A, adc_a_d )
        , ( 0x8B, adc_a_e )
        , ( 0x8C, adc_a_h )
        , ( 0x8D, adc_a_l )
        , ( 0x90, sub_b )
        , ( 0x91, sub_c )
        , ( 0x92, sub_d )
        , ( 0x93, sub_e )
        , ( 0x94, sub_h )
        , ( 0x95, sub_l )
        ]


singleByteMainRegs : Dict Int (MainWithIndexRegisters -> RegisterChangeData)
singleByteMainRegs =
    Dict.fromList
        [ ( 0x03, inc_bc )
        , ( 0x0B, dec_bc )
        , ( 0x13, inc_de )
        , ( 0x1B, dec_de )
        , ( 0x23, inc_hl )
        , ( 0x2B, dec_hl )
        , ( 0x41, ld_b_c )
        , ( 0x42, ld_b_d )
        , ( 0x43, ld_b_e )
        , ( 0x44, ld_b_h )
        , ( 0x45, ld_b_l )
        , ( 0x48, ld_c_b )
        , ( 0x4A, ld_c_d )
        , ( 0x4B, ld_c_e )
        , ( 0x4C, ld_c_h )
        , ( 0x4D, ld_c_l )
        , ( 0x50, ld_d_b )
        , ( 0x51, ld_d_c )
        , ( 0x53, ld_d_e )
        , ( 0x54, ld_d_h )
        , ( 0x55, ld_d_l )
        , ( 0x58, ld_e_b )
        , ( 0x59, ld_e_c )
        , ( 0x5A, ld_e_d )
        , ( 0x5C, ld_e_h )
        , ( 0x5D, ld_e_l )
        , ( 0x60, ld_h_b )
        , ( 0x61, ld_h_c )
        , ( 0x62, ld_h_d )
        , ( 0x63, ld_h_e )
        , ( 0x65, ld_h_l )
        , ( 0x68, ld_l_b )
        , ( 0x69, ld_l_c )
        , ( 0x6A, ld_l_d )
        , ( 0x6B, ld_l_e )
        , ( 0x6C, ld_l_h )
        , ( 0x78, ld_a_b )
        , ( 0x79, ld_a_c )
        , ( 0x7A, ld_a_d )
        , ( 0x7B, ld_a_e )
        , ( 0x7C, ld_a_h )
        , ( 0x7D, ld_a_l )
        ]


inc_bc : MainWithIndexRegisters -> RegisterChangeData
inc_bc z80_main =
    -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
    let
        changes =
            if z80_main.c == 0xFF then
                ChangeRegisterBC (Bitwise.and (z80_main.b + 1) 0xFF) 0

            else
                ChangeRegisterC (z80_main.c + 1)
    in
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c } } |> add_cpu_time 2
    { changes = changes, cpu_time = Just 2 }


inc_b : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_b z80_main z80_flags =
    -- case 0x04: B=inc(B); break;
    let
        new_b =
            inc z80_main.b z80_flags
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    --FlagsWithMain new_b.flags { z80_main | b = new_b.value }
    { changes = FlagsWithBRegister new_b.flags new_b.value, cpu_time = Nothing }


dec_b : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_b z80_main z80_flags =
    -- case 0x05: B=dec(B); break;
    let
        new_b =
            dec z80_main.b z80_flags
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    --FlagsWithMain new_b.flags { z80_main | b = new_b.value }
    { changes = FlagsWithBRegister new_b.flags new_b.value, cpu_time = Nothing }


dec_bc : MainWithIndexRegisters -> RegisterChangeData
dec_bc z80_main =
    -- case 0x0B: if(--C<0) B=B-1&(C=0xFF); time+=2; break;
    let
        tmp_c =
            z80_main.c - 1

        changes =
            if tmp_c < 0 then
                ChangeRegisterBC (Bitwise.and (z80_main.b - 1) 0xFF) 0xFF

            else
                ChangeRegisterC tmp_c
    in
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c }} |> add_cpu_time 2
    { changes = changes, cpu_time = Just 2 }


inc_c : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_c z80_main z80_flags =
    -- case 0x0C: C=inc(C); break;
    let
        new_c =
            inc z80_main.c z80_flags
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    { changes = FlagsWithCRegister new_c.flags new_c.value, cpu_time = Nothing }


dec_c : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_c z80_main z80_flags =
    -- case 0x0D: C=dec(C); break;
    let
        new_c =
            dec z80_main.c z80_flags
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    { changes = FlagsWithCRegister new_c.flags new_c.value, cpu_time = Nothing }


inc_de : MainWithIndexRegisters -> RegisterChangeData
inc_de z80_main =
    -- case 0x13: if(++E==256) {D=D+1&0xFF;E=0;} time+=2; break;
    let
        tmp_e =
            z80_main.e + 1

        changes =
            if tmp_e == 256 then
                ChangeRegisterDE (Bitwise.and (z80_main.d + 1) 0xFF) 0

            else
                ChangeRegisterE tmp_e

        --env_1 =
        --    z80.env |> addCpuTimeEnv 2
        --main_1 =
        --    { z80_main | d = reg_d, e = reg_e }
    in
    --{ z80 | env = env_1, main = main_1 }
    --MainRegsWithEnv main_1 env_1
    { changes = changes, cpu_time = Just 2 }


inc_d : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_d z80_main z80_flags =
    -- case 0x14: D=inc(D); break;
    let
        new_d =
            inc z80_main.d z80_flags

        changes =
            FlagsWithDRegister new_d.flags new_d.value
    in
    { changes = changes, cpu_time = Nothing }


dec_d : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_d z80_main z80_flags =
    -- case 0x15: D=dec(D); break;
    let
        new_d =
            dec z80_main.d z80_flags
    in
    --{ z80 | flags = new_d.flags, main = main_1 }
    { changes = FlagsWithDRegister new_d.flags new_d.value, cpu_time = Nothing }


dec_de : MainWithIndexRegisters -> RegisterChangeData
dec_de z80_main =
    -- case 0x1B: if(--E<0) D=D-1&(E=0xFF); time+=2; break;
    let
        tmp_e =
            z80_main.e - 1

        changes =
            if tmp_e < 0 then
                ChangeRegisterDE (Bitwise.and (z80_main.d - 1) 0xFF) 0xFF

            else
                ChangeRegisterE tmp_e
    in
    { changes = changes, cpu_time = Just 2 }


inc_e : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_e z80_main z80_flags =
    -- case 0x1C: E=inc(E); break;
    let
        new_e =
            inc z80_main.e z80_flags
    in
    --{ z80 | flags = new_e.flags, main = { z80_main | e = new_e.value } }
    { changes = FlagsWithERegister new_e.flags new_e.value, cpu_time = Nothing }


dec_e : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_e z80_main z80_flags =
    -- case 0x1D: E=dec(E); break;
    let
        new_e =
            dec z80_main.e z80_flags
    in
    { changes = FlagsWithERegister new_e.flags new_e.value, cpu_time = Nothing }


inc_hl : MainWithIndexRegisters -> RegisterChangeData
inc_hl z80_main =
    -- case 0x23: HL=(char)(HL+1); time+=2; break;
    -- case 0x23: xy=(char)(xy+1); time+=2; break;
    { changes = ChangeRegisterHL (Bitwise.and (z80_main.hl + 1) 0xFFFF), cpu_time = Just 2 }


inc_h : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_h z80_main z80_flags =
    -- case 0x24: HL=HL&0xFF|inc(HL>>>8)<<8; break;
    -- case 0x24: xy=xy&0xFF|inc(xy>>>8)<<8; break;
    let
        value =
            inc (shiftRightBy8 z80_main.hl) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 value.value)
    in
    --{ z80_1 | main = main }
    { changes = FlagsWithHLRegister value.flags new_xy, cpu_time = Nothing }


dec_h : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_h z80_main z80_flags =
    -- case 0x25: HL=HL&0xFF|dec(HL>>>8)<<8; break;
    -- case 0x25: xy=xy&0xFF|dec(xy>>>8)<<8; break;
    let
        value =
            dec (shiftRightBy8 z80_main.hl) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 value.value)
    in
    { changes = FlagsWithHLRegister value.flags new_xy, cpu_time = Nothing }


dec_hl : MainWithIndexRegisters -> RegisterChangeData
dec_hl z80_main =
    -- case 0x2B: HL=(char)(HL-1); time+=2; break;
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    let
        new_xy =
            Bitwise.and (z80_main.hl - 1) 0xFFFF
    in
    { changes = ChangeRegisterHL new_xy, cpu_time = Just 2 }


inc_l : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_l z80_main z80_flags =
    -- case 0x2C: HL=HL&0xFF00|inc(HL&0xFF); break;
    -- case 0x2C: xy=xy&0xFF00|inc(xy&0xFF); break;
    let
        h =
            Bitwise.and z80_main.hl 0xFF00

        l =
            inc (Bitwise.and z80_main.hl 0xFF) z80_flags

        --z80_1 = { z80 | flags = l.flags }
        new_xy =
            Bitwise.or h l.value
    in
    --{ z80_1 | main = main }
    { changes = HLRegister new_xy, cpu_time = Nothing }


dec_l : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_l z80_main z80_flags =
    -- case 0x2D: HL=HL&0xFF00|dec(HL&0xFF); break;
    -- case 0x2D: xy=xy&0xFF00|dec(xy&0xFF); break;
    let
        h =
            Bitwise.and z80_main.hl 0xFF00

        l =
            dec (Bitwise.and z80_main.hl 0xFF) z80_flags

        --new_z80 = { z80 | flags = l.flags }
        new_xy =
            Bitwise.or h l.value
    in
    --{ new_z80 | main = main }
    { changes = HLRegister new_xy, cpu_time = Nothing }


ld_b_c : MainWithIndexRegisters -> RegisterChangeData
ld_b_c z80_main =
    -- case 0x41: B=C; break;
    --z80 |> set_b z80.main.c
    { changes = ChangeRegisterB z80_main.c, cpu_time = Nothing }


ld_b_d : MainWithIndexRegisters -> RegisterChangeData
ld_b_d z80_main =
    -- case 0x42: B=D; break;
    --z80 |> set_b z80.main.d
    { changes = ChangeRegisterB z80_main.d, cpu_time = Nothing }


ld_b_e : MainWithIndexRegisters -> RegisterChangeData
ld_b_e z80_main =
    -- case 0x43: B=E; break;
    --z80 |> set_b z80.main.e
    { changes = ChangeRegisterB z80_main.e, cpu_time = Nothing }


ld_b_h : MainWithIndexRegisters -> RegisterChangeData
ld_b_h z80_main =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    { changes = ChangeRegisterB (shiftRightBy8 z80_main.hl), cpu_time = Nothing }


ld_b_l : MainWithIndexRegisters -> RegisterChangeData
ld_b_l z80_main =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    { changes = ChangeRegisterB (Bitwise.and z80_main.hl 0xFF), cpu_time = Nothing }


ld_c_b : MainWithIndexRegisters -> RegisterChangeData
ld_c_b z80_main =
    -- case 0x48: C=B; break;
    --z80 |> set_c z80.main.b
    { changes = ChangeRegisterC z80_main.b, cpu_time = Nothing }


ld_c_d : MainWithIndexRegisters -> RegisterChangeData
ld_c_d z80_main =
    -- case 0x4A: C=D; break;
    --z80 |> set_c z80.main.d
    { changes = ChangeRegisterC z80_main.d, cpu_time = Nothing }


add_hl_hl : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
add_hl_hl z80_main z80_flags =
    -- case 0x29: HL=add16(HL,HL); break;
    -- case 0x29: xy=add16(xy,xy); break;
    let
        new_xy =
            add16 z80_main.hl z80_main.hl z80_flags
    in
    --{ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time
    { changes = FlagsWithHLRegister new_xy.flags new_xy.value, cpu_time = Just new_xy.time }


ld_c_e : MainWithIndexRegisters -> RegisterChangeData
ld_c_e z80_main =
    -- case 0x4B: C=E; break;
    --z80 |> set_c z80.main.e
    { changes = ChangeRegisterC z80_main.e, cpu_time = Nothing }


ld_c_h : MainWithIndexRegisters -> RegisterChangeData
ld_c_h z80_main =
    -- case 0x4C: C=HL>>>8; break;
    --z80 |> set_c (get_h ixiyhl z80.main)
    { changes = ChangeRegisterC (shiftRightBy8 z80_main.hl), cpu_time = Nothing }


ld_c_l : MainWithIndexRegisters -> RegisterChangeData
ld_c_l z80_main =
    -- case 0x4D: C=HL&0xFF; break;
    --z80 |> set_c (get_l ixiyhl z80.main)
    { changes = ChangeRegisterC (Bitwise.and z80_main.hl 0xFF), cpu_time = Nothing }


add_hl_bc : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
add_hl_bc z80_main z80_flags =
    --case 0x09: HL=add16(HL,B<<8|C); break;
    --case 0x09: xy=add16(xy,B<<8|C); break;
    let
        xy =
            z80_main.hl

        new_xy =
            add16 xy (get_bc z80_main) z80_flags

        --new_z80 =
        --    set_xy new_xy.value ixiyhl z80.main
    in
    --Whole ({ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time)
    --FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time
    { changes = FlagsWithHLRegister new_xy.flags new_xy.value, cpu_time = Just new_xy.time }


ld_d_b : MainWithIndexRegisters -> RegisterChangeData
ld_d_b z80_main =
    -- case 0x50: D=B; break;
    --z80 |> set_d z80.main.b
    { changes = ChangeRegisterD z80_main.b, cpu_time = Nothing }


ld_d_c : MainWithIndexRegisters -> RegisterChangeData
ld_d_c z80_main =
    -- case 0x51: D=C; break;
    --z80 |> set_d z80.main.c
    { changes = ChangeRegisterD z80_main.c, cpu_time = Nothing }


ld_d_e : MainWithIndexRegisters -> RegisterChangeData
ld_d_e z80_main =
    -- case 0x53: D=E; break;
    --z80 |> set_d z80.main.e
    { changes = ChangeRegisterD z80_main.e, cpu_time = Nothing }


ld_e_b : MainWithIndexRegisters -> RegisterChangeData
ld_e_b z80_main =
    -- case 0x58: E=B; break;
    --z80 |> set_e z80.main.b
    { changes = ChangeRegisterE z80_main.b, cpu_time = Nothing }


ld_e_c : MainWithIndexRegisters -> RegisterChangeData
ld_e_c z80_main =
    -- case 0x59: E=C; break;
    --z80 |> set_e z80.main.c
    { changes = ChangeRegisterE z80_main.c, cpu_time = Nothing }


ld_e_d : MainWithIndexRegisters -> RegisterChangeData
ld_e_d z80_main =
    -- case 0x5A: E=D; break;
    --z80 |> set_e z80.main.d
    { changes = ChangeRegisterE z80_main.d, cpu_time = Nothing }


ld_e_h : MainWithIndexRegisters -> RegisterChangeData
ld_e_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    --z80 |> set_e (get_h ixiyhl z80.main)
    { changes = ChangeRegisterE (shiftRightBy8 z80_main.hl), cpu_time = Nothing }


ld_e_l : MainWithIndexRegisters -> RegisterChangeData
ld_e_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    --z80 |> set_e (get_l ixiyhl z80.main)
    { changes = ChangeRegisterE (Bitwise.and z80_main.hl 0xFF), cpu_time = Nothing }


ld_d_h : MainWithIndexRegisters -> RegisterChangeData
ld_d_h z80_main =
    -- case 0x54: D=HL>>>8; break;
    --z80 |> set_d (get_h ixiyhl z80.main)
    { changes = ChangeRegisterD (shiftRightBy8 z80_main.hl), cpu_time = Nothing }


ld_d_l : MainWithIndexRegisters -> RegisterChangeData
ld_d_l z80_main =
    -- case 0x55: D=HL&0xFF; break;
    --z80 |> set_d (get_l ixiyhl z80.main)
    { changes = ChangeRegisterD (Bitwise.and z80_main.hl 0xFF), cpu_time = Nothing }


ld_h_b : MainWithIndexRegisters -> RegisterChangeData
ld_h_b z80_main =
    -- case 0x60: HL=HL&0xFF|B<<8; break;
    -- case 0x60: xy=xy&0xFF|B<<8; break;
    --z80 |> set_h_z80 z80.main.b ixiyhl
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 z80_main.b)), cpu_time = Nothing }


ld_h_c : MainWithIndexRegisters -> RegisterChangeData
ld_h_c z80_main =
    -- case 0x61: HL=HL&0xFF|C<<8; break;
    -- case 0x61: xy=xy&0xFF|C<<8; break;
    --z80 |> set_h_z80 z80.main.c ixiyhl
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 z80_main.c)), cpu_time = Nothing }


ld_h_d : MainWithIndexRegisters -> RegisterChangeData
ld_h_d z80_main =
    -- case 0x62: HL=HL&0xFF|D<<8; break;
    -- case 0x62: xy=xy&0xFF|D<<8; break;
    --z80 |> set_h_z80 z80.main.d ixiyhl
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 z80_main.d)), cpu_time = Nothing }


ld_h_e : MainWithIndexRegisters -> RegisterChangeData
ld_h_e z80_main =
    -- case 0x63: HL=HL&0xFF|E<<8; break;
    -- case 0x63: xy=xy&0xFF|E<<8; break;
    --z80 |> set_h_z80 z80.main.e ixiyhl
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 z80_main.e)), cpu_time = Nothing }


ld_h_l : MainWithIndexRegisters -> RegisterChangeData
ld_h_l z80_main =
    -- case 0x65: HL=HL&0xFF|(HL&0xFF)<<8; break;
    -- case 0x65: xy=xy&0xFF|(xy&0xFF)<<8; break;
    --z80 |> set_h_z80 (get_l ixiyhl z80.main) ixiyhl
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 (Bitwise.and z80_main.hl 0xFF))), cpu_time = Nothing }


ld_l_b : MainWithIndexRegisters -> RegisterChangeData
ld_l_b z80_main =
    -- case 0x68: HL=HL&0xFF00|B; break;
    -- case 0x68: xy=xy&0xFF00|B; break;
    --z80 |> set_l_z80 z80.main.b ixiyhl
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF00) z80_main.b), cpu_time = Nothing }


ld_l_c : MainWithIndexRegisters -> RegisterChangeData
ld_l_c z80_main =
    -- case 0x69: HL=HL&0xFF00|C; break;
    -- case 0x69: xy=xy&0xFF00|C; break;
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF00) z80_main.c), cpu_time = Nothing }


ld_l_d : MainWithIndexRegisters -> RegisterChangeData
ld_l_d z80_main =
    -- case 0x6A: HL=HL&0xFF00|D; break;
    -- case 0x6A: xy=xy&0xFF00|D; break;
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF00) z80_main.d), cpu_time = Nothing }


ld_l_e : MainWithIndexRegisters -> RegisterChangeData
ld_l_e z80_main =
    -- case 0x6B: HL=HL&0xFF00|E; break;
    -- case 0x6B: xy=xy&0xFF00|E; break;
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF00) z80_main.e), cpu_time = Nothing }


ld_l_h : MainWithIndexRegisters -> RegisterChangeData
ld_l_h z80_main =
    -- case 0x6C: HL=HL&0xFF00|HL>>>8; break;
    -- case 0x6C: xy=xy&0xFF00|xy>>>8; break;
    { changes = ChangeRegisterHL (Bitwise.or (Bitwise.and z80_main.hl 0xFF00) (shiftRightBy8 z80_main.hl)), cpu_time = Nothing }


ld_a_b : MainWithIndexRegisters -> RegisterChangeData
ld_a_b z80_main =
    -- case 0x78: A=B; break;
    --z80 |> set_a z80.main.b
    { changes = ChangeRegisterA z80_main.b, cpu_time = Nothing }


ld_a_c : MainWithIndexRegisters -> RegisterChangeData
ld_a_c z80_main =
    -- case 0x79: A=C; break;
    --z80 |> set_a z80.main.c
    { changes = ChangeRegisterA z80_main.c, cpu_time = Nothing }


ld_a_d : MainWithIndexRegisters -> RegisterChangeData
ld_a_d z80_main =
    -- case 0x7A: A=D; break;
    --z80 |> set_a z80.main.d
    { changes = ChangeRegisterA z80_main.d, cpu_time = Nothing }


ld_a_e : MainWithIndexRegisters -> RegisterChangeData
ld_a_e z80_main =
    -- case 0x7B: A=E; break;
    --z80 |> set_a z80.main.e
    { changes = ChangeRegisterA z80_main.e, cpu_time = Nothing }


ld_a_h : MainWithIndexRegisters -> RegisterChangeData
ld_a_h z80_main =
    -- case 0x7C: A=HL>>>8; break;
    -- case 0x7C: A=xy>>>8; break;
    --z80 |> set_a (get_h ixiyhl z80.main)
    { changes = ChangeRegisterA (shiftRightBy8 z80_main.hl), cpu_time = Nothing }


ld_a_l : MainWithIndexRegisters -> RegisterChangeData
ld_a_l z80_main =
    -- case 0x7D: A=HL&0xFF; break;
    -- case 0x7D: A=xy&0xFF; break;
    --z80 |> set_a (get_l ixiyhl z80.main)
    { changes = ChangeRegisterA (Bitwise.and z80_main.hl 0xFF), cpu_time = Nothing }


add_a_b : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
add_a_b z80_main z80_flags =
    -- case 0x80: add(B); break;
    --z80 |> set_flag_regs (z80_add z80.main.b z80.flags)
    { changes = Z80ChangeFlags (z80_add z80_main.b z80_flags), cpu_time = Nothing }


add_a_c : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
add_a_c z80_main z80_flags =
    -- case 0x81: add(C); break;
    --z80 |> set_flag_regs (z80_add z80.main.c z80.flags)
    { changes = Z80ChangeFlags (z80_add z80_main.c z80_flags), cpu_time = Nothing }


add_a_d : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
add_a_d z80_main z80_flags =
    -- case 0x82: add(D); break;
    --z80 |> set_flag_regs (z80_add z80.main.d z80.flags)
    { changes = Z80ChangeFlags (z80_add z80_main.d z80_flags), cpu_time = Nothing }


add_a_e : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
add_a_e z80_main z80_flags =
    -- case 0x83: add(E); break;
    --z80 |> set_flag_regs (z80_add z80.main.e z80.flags)
    { changes = Z80ChangeFlags (z80_add z80_main.e z80_flags), cpu_time = Nothing }


add_a_h : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
add_a_h z80_main z80_flags =
    -- case 0x84: add(HL>>>8); break;
    -- case 0x84: add(xy>>>8); break;
    --z80 |> set_flag_regs (z80_add (get_h ixiyhl z80.main) z80.flags)
    { changes = Z80ChangeFlags (z80_add (shiftRightBy8 z80_main.hl) z80_flags), cpu_time = Nothing }


add_a_l : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
add_a_l z80_main z80_flags =
    -- case 0x85: add(HL&0xFF); break;
    -- case 0x85: add(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_add (get_l ixiyhl z80.main) z80.flags)
    { changes = Z80ChangeFlags (z80_add (Bitwise.and z80_main.hl 0xFF) z80_flags), cpu_time = Nothing }


adc_a_b : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
adc_a_b z80_main z80_flags =
    -- case 0x88: adc(B); break;
    { changes = Z80ChangeFlags (z80_flags |> adc z80_main.b), cpu_time = Nothing }


adc_a_c : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
adc_a_c z80_main z80_flags =
    -- case 0x89: adc(C); break;
    --z80 |> set_flag_regs (adc z80.main.c z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> adc z80_main.c), cpu_time = Nothing }


adc_a_d : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
adc_a_d z80_main z80_flags =
    -- case 0x8A: adc(D); break;
    --z80 |> set_flag_regs (adc z80.main.d z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> adc z80_main.d), cpu_time = Nothing }


adc_a_e : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
adc_a_e z80_main z80_flags =
    -- case 0x8B: adc(E); break;
    --z80 |> set_flag_regs (adc z80.main.e z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> adc z80_main.e), cpu_time = Nothing }


adc_a_h : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
adc_a_h z80_main z80_flags =
    -- case 0x84: add(HL>>>8); break;
    -- case 0x84: add(xy>>>8); break;
    --z80 |> set_flag_regs (z80_add (get_h ixiyhl z80.main) z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> adc (shiftRightBy8 z80_main.hl)), cpu_time = Nothing }


adc_a_l : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
adc_a_l z80_main z80_flags =
    -- case 0x85: add(HL&0xFF); break;
    -- case 0x85: add(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_add (get_l ixiyhl z80.main) z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> adc (Bitwise.and z80_main.hl 0xFF)), cpu_time = Nothing }


sub_b : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
sub_b z80_main z80_flags =
    -- case 0x90: sub(B); break;
    --z80 |> set_flag_regs (z80_sub z80.main.b z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> z80_sub z80_main.b), cpu_time = Nothing }


sub_c : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
sub_c z80_main z80_flags =
    -- case 0x91: sub(C); break;
    --z80 |> set_flag_regs (z80_sub z80.main.c z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> z80_sub z80_main.c), cpu_time = Nothing }


sub_d : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
sub_d z80_main z80_flags =
    -- case 0x92: sub(D); break;
    --z80 |> set_flag_regs (z80_sub z80.main.d z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> z80_sub z80_main.d), cpu_time = Nothing }


sub_e : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
sub_e z80_main z80_flags =
    -- case 0x93: sub(E); break;
    --z80 |> set_flag_regs (z80_sub z80.main.e z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> z80_sub z80_main.e), cpu_time = Nothing }


sub_h : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
sub_h z80_main z80_flags =
    -- case 0x94: sub(HL>>>8); break;
    -- case 0x94: sub(xy>>>8); break;
    --z80 |> set_flag_regs (z80_sub (get_h ixiyhl z80.main) z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> z80_sub (shiftRightBy8 z80_main.hl)), cpu_time = Nothing }


sub_l : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
sub_l z80_main z80_flags =
    -- case 0x95: sub(HL&0xFF); break;
    -- case 0x95: sub(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_sub (get_l ixiyhl z80.main) z80.flags)
    { changes = Z80ChangeFlags (z80_flags |> z80_sub (Bitwise.and z80_main.hl 0xFF)), cpu_time = Nothing }
