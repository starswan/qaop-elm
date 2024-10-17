module SimpleSingleByte exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement(..))
import Dict exposing (Dict)
import RegisterChange exposing (RegisterChange(..))
import Utils exposing (shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (Z80Change(..))
import Z80Flags exposing (FlagRegisters, adc, add16, dec, inc, sbc, z80_add, z80_and, z80_cp, z80_or, z80_sub, z80_xor)
import Z80Types exposing (IXIYHL(..), MainRegisters, MainWithIndexRegisters, Z80, get_bc)


singleByteMainAndFlagRegisters : Dict Int (MainWithIndexRegisters -> FlagRegisters -> Z80Change)
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
        , ( 0x98, sbc_b )
        , ( 0x99, sbc_c )
        , ( 0x9A, sbc_d )
        , ( 0x9B, sbc_e )
        , ( 0x9C, sbc_h )
        , ( 0x9D, sbc_l )
        , ( 0xA0, and_b )
        , ( 0xA1, and_c )
        , ( 0xA2, and_d )
        , ( 0xA3, and_e )
        , ( 0xA4, and_h )
        , ( 0xA5, and_l )
        , ( 0xA8, xor_b )
        , ( 0xA9, xor_c )
        , ( 0xAA, xor_d )
        , ( 0xAB, xor_e )
        , ( 0xAC, xor_h )
        , ( 0xAD, xor_l )
        , ( 0xB0, or_b )
        , ( 0xB1, or_c )
        , ( 0xB2, or_d )
        , ( 0xB3, or_e )
        , ( 0xB4, or_h )
        , ( 0xB5, or_l )
        , ( 0xB8, cp_b )
        , ( 0xB9, cp_c )
        , ( 0xBA, cp_d )
        , ( 0xBB, cp_e )
        , ( 0xBC, cp_h )
        , ( 0xBD, cp_l )
        ]


singleByteMainRegs : Dict Int (MainWithIndexRegisters -> RegisterChange)
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


inc_bc : MainWithIndexRegisters -> RegisterChange
inc_bc z80_main =
    -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
    ----{ z80 | main = { z80_main | b = reg_b, c = reg_c } } |> add_cpu_time 2
    if z80_main.c == 0xFF then
        ChangeRegisterBC (Bitwise.and (z80_main.b + 1) 0xFF) 0 (CpuTimeIncrement 2)

    else
        ChangeRegisterCWithTime (z80_main.c + 1) (CpuTimeIncrement 2)


inc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_b z80_main z80_flags =
    -- case 0x04: B=inc(B); break;
    let
        new_b =
            inc z80_main.b z80_flags
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    FlagsWithBRegister new_b.flags new_b.value


dec_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_b z80_main z80_flags =
    -- case 0x05: B=dec(B); break;
    let
        new_b =
            dec z80_main.b z80_flags
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    FlagsWithBRegister new_b.flags new_b.value


dec_bc : MainWithIndexRegisters -> RegisterChange
dec_bc z80_main =
    -- case 0x0B: if(--C<0) B=B-1&(C=0xFF); time+=2; break;
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c }} |> add_cpu_time 2
    let
        tmp_c =
            z80_main.c - 1
    in
    if tmp_c < 0 then
        ChangeRegisterBC (Bitwise.and (z80_main.b - 1) 0xFF) 0xFF (CpuTimeIncrement 2)

    else
        ChangeRegisterCWithTime tmp_c (CpuTimeIncrement 2)


inc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_c z80_main z80_flags =
    -- case 0x0C: C=inc(C); break;
    let
        new_c =
            inc z80_main.c z80_flags
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    FlagsWithCRegister new_c.flags new_c.value


dec_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_c z80_main z80_flags =
    -- case 0x0D: C=dec(C); break;
    let
        new_c =
            dec z80_main.c z80_flags
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    FlagsWithCRegister new_c.flags new_c.value


inc_de : MainWithIndexRegisters -> RegisterChange
inc_de z80_main =
    -- case 0x13: if(++E==256) {D=D+1&0xFF;E=0;} time+=2; break;
    let
        tmp_e =
            z80_main.e + 1
    in
    if tmp_e == 256 then
        ChangeRegisterDE (Bitwise.and (z80_main.d + 1) 0xFF) 0 (CpuTimeIncrement 2)

    else
        ChangeRegisterEWithTime tmp_e (CpuTimeIncrement 2)


inc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_d z80_main z80_flags =
    -- case 0x14: D=inc(D); break;
    let
        new_d =
            inc z80_main.d z80_flags
    in
    FlagsWithDRegister new_d.flags new_d.value


dec_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_d z80_main z80_flags =
    -- case 0x15: D=dec(D); break;
    let
        new_d =
            dec z80_main.d z80_flags
    in
    --{ z80 | flags = new_d.flags, main = main_1 }
    FlagsWithDRegister new_d.flags new_d.value


dec_de : MainWithIndexRegisters -> RegisterChange
dec_de z80_main =
    -- case 0x1B: if(--E<0) D=D-1&(E=0xFF); time+=2; break;
    let
        tmp_e =
            z80_main.e - 1
    in
    if tmp_e < 0 then
        ChangeRegisterDE (Bitwise.and (z80_main.d - 1) 0xFF) 0xFF (CpuTimeIncrement 2)

    else
        ChangeRegisterEWithTime tmp_e (CpuTimeIncrement 2)


inc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_e z80_main z80_flags =
    -- case 0x1C: E=inc(E); break;
    let
        new_e =
            inc z80_main.e z80_flags
    in
    --{ z80 | flags = new_e.flags, main = { z80_main | e = new_e.value } }
    FlagsWithERegister new_e.flags new_e.value


dec_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_e z80_main z80_flags =
    -- case 0x1D: E=dec(E); break;
    let
        new_e =
            dec z80_main.e z80_flags
    in
    FlagsWithERegister new_e.flags new_e.value


inc_hl : MainWithIndexRegisters -> RegisterChange
inc_hl z80_main =
    -- case 0x23: HL=(char)(HL+1); time+=2; break;
    -- case 0x23: xy=(char)(xy+1); time+=2; break;
    ChangeRegisterHL (Bitwise.and (z80_main.hl + 1) 0xFFFF) (CpuTimeIncrement 2)


inc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_h z80_main z80_flags =
    -- case 0x24: HL=HL&0xFF|inc(HL>>>8)<<8; break;
    -- case 0x24: xy=xy&0xFF|inc(xy>>>8)<<8; break;
    let
        value =
            inc (shiftRightBy8 z80_main.hl) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 value.value)
    in
    FlagsWithHLRegister value.flags new_xy 0


dec_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_h z80_main z80_flags =
    -- case 0x25: HL=HL&0xFF|dec(HL>>>8)<<8; break;
    -- case 0x25: xy=xy&0xFF|dec(xy>>>8)<<8; break;
    let
        value =
            dec (shiftRightBy8 z80_main.hl) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 value.value)
    in
    FlagsWithHLRegister value.flags new_xy 0


dec_hl : MainWithIndexRegisters -> RegisterChange
dec_hl z80_main =
    -- case 0x2B: HL=(char)(HL-1); time+=2; break;
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    let
        new_xy =
            Bitwise.and (z80_main.hl - 1) 0xFFFF
    in
    ChangeRegisterHL new_xy (CpuTimeIncrement 2)


inc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_l z80_main z80_flags =
    -- case 0x2C: HL=HL&0xFF00|inc(HL&0xFF); break;
    -- case 0x2C: xy=xy&0xFF00|inc(xy&0xFF); break;
    let
        h =
            Bitwise.and z80_main.hl 0xFF00

        l =
            inc (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_xy =
            Bitwise.or h l.value
    in
    HLRegister new_xy


dec_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_l z80_main z80_flags =
    -- case 0x2D: HL=HL&0xFF00|dec(HL&0xFF); break;
    -- case 0x2D: xy=xy&0xFF00|dec(xy&0xFF); break;
    let
        h =
            Bitwise.and z80_main.hl 0xFF00

        l =
            dec (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_xy =
            Bitwise.or h l.value
    in
    HLRegister new_xy


ld_b_c : MainWithIndexRegisters -> RegisterChange
ld_b_c z80_main =
    -- case 0x41: B=C; break;
    --z80 |> set_b z80.main.c
    ChangeRegisterB z80_main.c


ld_b_d : MainWithIndexRegisters -> RegisterChange
ld_b_d z80_main =
    -- case 0x42: B=D; break;
    --z80 |> set_b z80.main.d
    ChangeRegisterB z80_main.d


ld_b_e : MainWithIndexRegisters -> RegisterChange
ld_b_e z80_main =
    -- case 0x43: B=E; break;
    --z80 |> set_b z80.main.e
    ChangeRegisterB z80_main.e


ld_b_h : MainWithIndexRegisters -> RegisterChange
ld_b_h z80_main =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    ChangeRegisterB (shiftRightBy8 z80_main.hl)


ld_b_l : MainWithIndexRegisters -> RegisterChange
ld_b_l z80_main =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    ChangeRegisterB (Bitwise.and z80_main.hl 0xFF)


ld_c_b : MainWithIndexRegisters -> RegisterChange
ld_c_b z80_main =
    -- case 0x48: C=B; break;
    --z80 |> set_c z80.main.b
    ChangeRegisterC z80_main.b


ld_c_d : MainWithIndexRegisters -> RegisterChange
ld_c_d z80_main =
    -- case 0x4A: C=D; break;
    --z80 |> set_c z80.main.d
    ChangeRegisterC z80_main.d


add_hl_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_hl_hl z80_main z80_flags =
    -- case 0x29: HL=add16(HL,HL); break;
    -- case 0x29: xy=add16(xy,xy); break;
    let
        new_xy =
            add16 z80_main.hl z80_main.hl z80_flags
    in
    --{ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time
    FlagsWithHLRegister new_xy.flags new_xy.value new_xy.time


ld_c_e : MainWithIndexRegisters -> RegisterChange
ld_c_e z80_main =
    -- case 0x4B: C=E; break;
    --z80 |> set_c z80.main.e
    ChangeRegisterC z80_main.e


ld_c_h : MainWithIndexRegisters -> RegisterChange
ld_c_h z80_main =
    -- case 0x4C: C=HL>>>8; break;
    --z80 |> set_c (get_h ixiyhl z80.main)
    ChangeRegisterC (shiftRightBy8 z80_main.hl)


ld_c_l : MainWithIndexRegisters -> RegisterChange
ld_c_l z80_main =
    -- case 0x4D: C=HL&0xFF; break;
    --z80 |> set_c (get_l ixiyhl z80.main)
    ChangeRegisterC (Bitwise.and z80_main.hl 0xFF)


add_hl_bc : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_hl_bc z80_main z80_flags =
    --case 0x09: HL=add16(HL,B<<8|C); break;
    --case 0x09: xy=add16(xy,B<<8|C); break;
    let
        xy =
            z80_main.hl

        new_xy =
            add16 xy (get_bc z80_main) z80_flags
    in
    FlagsWithHLRegister new_xy.flags new_xy.value new_xy.time


ld_d_b : MainWithIndexRegisters -> RegisterChange
ld_d_b z80_main =
    -- case 0x50: D=B; break;
    --z80 |> set_d z80.main.b
    ChangeRegisterD z80_main.b


ld_d_c : MainWithIndexRegisters -> RegisterChange
ld_d_c z80_main =
    -- case 0x51: D=C; break;
    --z80 |> set_d z80.main.c
    ChangeRegisterD z80_main.c


ld_d_e : MainWithIndexRegisters -> RegisterChange
ld_d_e z80_main =
    -- case 0x53: D=E; break;
    --z80 |> set_d z80.main.e
    ChangeRegisterD z80_main.e


ld_e_b : MainWithIndexRegisters -> RegisterChange
ld_e_b z80_main =
    -- case 0x58: E=B; break;
    --z80 |> set_e z80.main.b
    ChangeRegisterE z80_main.b


ld_e_c : MainWithIndexRegisters -> RegisterChange
ld_e_c z80_main =
    -- case 0x59: E=C; break;
    --z80 |> set_e z80.main.c
    ChangeRegisterE z80_main.c


ld_e_d : MainWithIndexRegisters -> RegisterChange
ld_e_d z80_main =
    -- case 0x5A: E=D; break;
    --z80 |> set_e z80.main.d
    ChangeRegisterE z80_main.d


ld_e_h : MainWithIndexRegisters -> RegisterChange
ld_e_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    --z80 |> set_e (get_h ixiyhl z80.main)
    ChangeRegisterE (shiftRightBy8 z80_main.hl)


ld_e_l : MainWithIndexRegisters -> RegisterChange
ld_e_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    --z80 |> set_e (get_l ixiyhl z80.main)
    ChangeRegisterE (Bitwise.and z80_main.hl 0xFF)


ld_d_h : MainWithIndexRegisters -> RegisterChange
ld_d_h z80_main =
    -- case 0x54: D=HL>>>8; break;
    --z80 |> set_d (get_h ixiyhl z80.main)
    ChangeRegisterD (shiftRightBy8 z80_main.hl)


ld_d_l : MainWithIndexRegisters -> RegisterChange
ld_d_l z80_main =
    -- case 0x55: D=HL&0xFF; break;
    --z80 |> set_d (get_l ixiyhl z80.main)
    ChangeRegisterD (Bitwise.and z80_main.hl 0xFF)


ld_h_b : MainWithIndexRegisters -> RegisterChange
ld_h_b z80_main =
    -- case 0x60: HL=HL&0xFF|B<<8; break;
    -- case 0x60: xy=xy&0xFF|B<<8; break;
    --z80 |> set_h_z80 z80.main.b ixiyhl
    ChangeRegisterH z80_main.b


ld_h_c : MainWithIndexRegisters -> RegisterChange
ld_h_c z80_main =
    -- case 0x61: HL=HL&0xFF|C<<8; break;
    -- case 0x61: xy=xy&0xFF|C<<8; break;
    --z80 |> set_h_z80 z80.main.c ixiyhl
    ChangeRegisterH z80_main.c


ld_h_d : MainWithIndexRegisters -> RegisterChange
ld_h_d z80_main =
    -- case 0x62: HL=HL&0xFF|D<<8; break;
    -- case 0x62: xy=xy&0xFF|D<<8; break;
    --z80 |> set_h_z80 z80.main.d ixiyhl
    ChangeRegisterH z80_main.d


ld_h_e : MainWithIndexRegisters -> RegisterChange
ld_h_e z80_main =
    -- case 0x63: HL=HL&0xFF|E<<8; break;
    -- case 0x63: xy=xy&0xFF|E<<8; break;
    --z80 |> set_h_z80 z80.main.e ixiyhl
    ChangeRegisterH z80_main.e


ld_h_l : MainWithIndexRegisters -> RegisterChange
ld_h_l z80_main =
    -- case 0x65: HL=HL&0xFF|(HL&0xFF)<<8; break;
    -- case 0x65: xy=xy&0xFF|(xy&0xFF)<<8; break;
    --z80 |> set_h_z80 (get_l ixiyhl z80.main) ixiyhl
    ChangeRegisterH (Bitwise.and z80_main.hl 0xFF)


ld_l_b : MainWithIndexRegisters -> RegisterChange
ld_l_b z80_main =
    -- case 0x68: HL=HL&0xFF00|B; break;
    -- case 0x68: xy=xy&0xFF00|B; break;
    --z80 |> set_l_z80 z80.main.b ixiyhl
    ChangeRegisterL z80_main.b


ld_l_c : MainWithIndexRegisters -> RegisterChange
ld_l_c z80_main =
    -- case 0x69: HL=HL&0xFF00|C; break;
    -- case 0x69: xy=xy&0xFF00|C; break;
    ChangeRegisterL z80_main.c


ld_l_d : MainWithIndexRegisters -> RegisterChange
ld_l_d z80_main =
    -- case 0x6A: HL=HL&0xFF00|D; break;
    -- case 0x6A: xy=xy&0xFF00|D; break;
    ChangeRegisterL z80_main.d


ld_l_e : MainWithIndexRegisters -> RegisterChange
ld_l_e z80_main =
    -- case 0x6B: HL=HL&0xFF00|E; break;
    -- case 0x6B: xy=xy&0xFF00|E; break;
    ChangeRegisterL z80_main.e


ld_l_h : MainWithIndexRegisters -> RegisterChange
ld_l_h z80_main =
    -- case 0x6C: HL=HL&0xFF00|HL>>>8; break;
    -- case 0x6C: xy=xy&0xFF00|xy>>>8; break;
    ChangeRegisterL (shiftRightBy8 z80_main.hl)


ld_a_b : MainWithIndexRegisters -> RegisterChange
ld_a_b z80_main =
    -- case 0x78: A=B; break;
    --z80 |> set_a z80.main.b
    ChangeRegisterA z80_main.b


ld_a_c : MainWithIndexRegisters -> RegisterChange
ld_a_c z80_main =
    -- case 0x79: A=C; break;
    --z80 |> set_a z80.main.c
    ChangeRegisterA z80_main.c


ld_a_d : MainWithIndexRegisters -> RegisterChange
ld_a_d z80_main =
    -- case 0x7A: A=D; break;
    --z80 |> set_a z80.main.d
    ChangeRegisterA z80_main.d


ld_a_e : MainWithIndexRegisters -> RegisterChange
ld_a_e z80_main =
    -- case 0x7B: A=E; break;
    --z80 |> set_a z80.main.e
    ChangeRegisterA z80_main.e


ld_a_h : MainWithIndexRegisters -> RegisterChange
ld_a_h z80_main =
    -- case 0x7C: A=HL>>>8; break;
    -- case 0x7C: A=xy>>>8; break;
    --z80 |> set_a (get_h ixiyhl z80.main)
    ChangeRegisterA (shiftRightBy8 z80_main.hl)


ld_a_l : MainWithIndexRegisters -> RegisterChange
ld_a_l z80_main =
    -- case 0x7D: A=HL&0xFF; break;
    -- case 0x7D: A=xy&0xFF; break;
    --z80 |> set_a (get_l ixiyhl z80.main)
    ChangeRegisterA (Bitwise.and z80_main.hl 0xFF)


add_a_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_b z80_main z80_flags =
    -- case 0x80: add(B); break;
    --z80 |> set_flag_regs (z80_add z80.main.b z80.flags)
    Z80ChangeFlags (z80_add z80_main.b z80_flags)


add_a_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_c z80_main z80_flags =
    -- case 0x81: add(C); break;
    --z80 |> set_flag_regs (z80_add z80.main.c z80.flags)
    Z80ChangeFlags (z80_add z80_main.c z80_flags)


add_a_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_d z80_main z80_flags =
    -- case 0x82: add(D); break;
    --z80 |> set_flag_regs (z80_add z80.main.d z80.flags)
    Z80ChangeFlags (z80_add z80_main.d z80_flags)


add_a_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_e z80_main z80_flags =
    -- case 0x83: add(E); break;
    --z80 |> set_flag_regs (z80_add z80.main.e z80.flags)
    Z80ChangeFlags (z80_add z80_main.e z80_flags)


add_a_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_h z80_main z80_flags =
    -- case 0x84: add(HL>>>8); break;
    -- case 0x84: add(xy>>>8); break;
    --z80 |> set_flag_regs (z80_add (get_h ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_add (shiftRightBy8 z80_main.hl) z80_flags)


add_a_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_l z80_main z80_flags =
    -- case 0x85: add(HL&0xFF); break;
    -- case 0x85: add(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_add (get_l ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_add (Bitwise.and z80_main.hl 0xFF) z80_flags)


adc_a_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_b z80_main z80_flags =
    -- case 0x88: adc(B); break;
    Z80ChangeFlags (z80_flags |> adc z80_main.b)


adc_a_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_c z80_main z80_flags =
    -- case 0x89: adc(C); break;
    --z80 |> set_flag_regs (adc z80.main.c z80.flags)
    Z80ChangeFlags (z80_flags |> adc z80_main.c)


adc_a_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_d z80_main z80_flags =
    -- case 0x8A: adc(D); break;
    --z80 |> set_flag_regs (adc z80.main.d z80.flags)
    Z80ChangeFlags (z80_flags |> adc z80_main.d)


adc_a_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_e z80_main z80_flags =
    -- case 0x8B: adc(E); break;
    --z80 |> set_flag_regs (adc z80.main.e z80.flags)
    Z80ChangeFlags (z80_flags |> adc z80_main.e)


adc_a_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_h z80_main z80_flags =
    -- case 0x84: add(HL>>>8); break;
    -- case 0x84: add(xy>>>8); break;
    --z80 |> set_flag_regs (z80_add (get_h ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> adc (shiftRightBy8 z80_main.hl))


adc_a_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_l z80_main z80_flags =
    -- case 0x85: add(HL&0xFF); break;
    -- case 0x85: add(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_add (get_l ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> adc (Bitwise.and z80_main.hl 0xFF))


sub_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_b z80_main z80_flags =
    -- case 0x90: sub(B); break;
    --z80 |> set_flag_regs (z80_sub z80.main.b z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub z80_main.b)


sub_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_c z80_main z80_flags =
    -- case 0x91: sub(C); break;
    --z80 |> set_flag_regs (z80_sub z80.main.c z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub z80_main.c)


sub_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_d z80_main z80_flags =
    -- case 0x92: sub(D); break;
    --z80 |> set_flag_regs (z80_sub z80.main.d z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub z80_main.d)


sub_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_e z80_main z80_flags =
    -- case 0x93: sub(E); break;
    --z80 |> set_flag_regs (z80_sub z80.main.e z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub z80_main.e)


sub_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_h z80_main z80_flags =
    -- case 0x94: sub(HL>>>8); break;
    -- case 0x94: sub(xy>>>8); break;
    --z80 |> set_flag_regs (z80_sub (get_h ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub (shiftRightBy8 z80_main.hl))


sub_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_l z80_main z80_flags =
    -- case 0x95: sub(HL&0xFF); break;
    -- case 0x95: sub(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_sub (get_l ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub (Bitwise.and z80_main.hl 0xFF))


sbc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_b z80_main z80_flags =
    -- case 0x98: sbc(B); break;
    --z80 |> set_flag_regs (sbc z80.main.b z80.flags)
    Z80ChangeFlags (z80_flags |> sbc z80_main.b)


sbc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_c z80_main z80_flags =
    -- case 0x99: sbc(C); break;
    --z80 |> set_flag_regs (sbc z80.main.c z80.flags)
    Z80ChangeFlags (z80_flags |> sbc z80_main.c)


sbc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_d z80_main z80_flags =
    -- case 0x9A: sbc(D); break;
    --z80 |> set_flag_regs (sbc z80.main.d z80.flags)
    Z80ChangeFlags (z80_flags |> sbc z80_main.d)


sbc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_e z80_main z80_flags =
    -- case 0x9B: sbc(E); break;
    --z80 |> set_flag_regs (sbc z80.main.e z80.flags)
    Z80ChangeFlags (z80_flags |> sbc z80_main.e)


sbc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_h z80_main z80_flags =
    -- case 0x9C: sbc(HL>>>8); break;
    -- case 0x9C: sbc(xy>>>8); break;
    --z80 |> set_flag_regs (sbc (get_h ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> sbc (shiftRightBy8 z80_main.hl))


sbc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_l z80_main z80_flags =
    -- case 0x9D: sbc(HL&0xFF); break;
    -- case 0x9D: sbc(xy&0xFF); break;
    --z80 |> set_flag_regs (sbc (get_l ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> sbc (Bitwise.and z80_main.hl 0xFF))


and_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_b z80_main z80_flags =
    -- case 0xA0: and(B); break;
    --z80 |> set_flag_regs (z80_and z80.main.b z80.flags)
    z80_flags |> z80_and z80_main.b |> Z80ChangeFlags


and_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_c z80_main z80_flags =
    -- case 0xA1: and(C); break;
    --z80 |> set_flag_regs (z80_and z80.main.c z80.flags)
    z80_flags |> z80_and z80_main.c |> Z80ChangeFlags


and_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_d z80_main z80_flags =
    -- case 0xA2: and(D); break;
    --z80 |> set_flag_regs (z80_and z80.main.d z80.flags)
    z80_flags |> z80_and z80_main.d |> Z80ChangeFlags


and_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_e z80_main z80_flags =
    -- case 0xA3: and(E); break;
    --z80 |> set_flag_regs (z80_and z80.main.e z80.flags)
    z80_flags |> z80_and z80_main.e |> Z80ChangeFlags


and_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_h z80_main z80_flags =
    -- case 0xA4: and(HL>>>8); break;
    -- case 0xA4: and(xy>>>8); break;
    --z80 |> set_flag_regs (z80_and (get_h ixiyhl z80.main) z80.flags)
    z80_flags |> z80_and (shiftRightBy8 z80_main.hl) |> Z80ChangeFlags


and_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_l z80_main z80_flags =
    -- case 0xA5: and(HL&0xFF); break;
    -- case 0xA5: and(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_and (get_l ixiyhl z80.main) z80.flags)
    z80_flags |> z80_and (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


xor_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_b z80_main z80_flags =
    -- case 0xA8: xor(B); break;
    z80_flags |> z80_xor z80_main.b |> Z80ChangeFlags


xor_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_c z80_main z80_flags =
    -- case 0xA9: xor(C); break;
    z80_flags |> z80_xor z80_main.c |> Z80ChangeFlags


xor_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_d z80_main z80_flags =
    -- case 0xAA: xor(D); break;
    z80_flags |> z80_xor z80_main.d |> Z80ChangeFlags


xor_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_e z80_main z80_flags =
    -- case 0xAB: xor(E); break;
    z80_flags |> z80_xor z80_main.e |> Z80ChangeFlags


xor_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_h z80_main z80_flags =
    -- case 0xAC: xor(HL>>>8); break;
    -- case 0xAC: xor(xy>>>8); break;
    z80_flags |> z80_xor (shiftRightBy8 z80_main.hl) |> Z80ChangeFlags


xor_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_l z80_main z80_flags =
    -- case 0xAD: xor(HL&0xFF); break;
    -- case 0xAD: xor(xy&0xFF); break;
    z80_flags |> z80_xor (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


or_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_b z80_main z80_flags =
    -- case 0xB0: or(B); break;
    --z80 |> set_flag_regs (z80_or z80.main.b z80.flags)
    z80_flags |> z80_or z80_main.b |> Z80ChangeFlags


or_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_c z80_main z80_flags =
    -- case 0xB1: or(C); break;
    z80_flags |> z80_or z80_main.c |> Z80ChangeFlags


or_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_d z80_main z80_flags =
    -- case 0xB2: or(D); break;
    z80_flags |> z80_or z80_main.d |> Z80ChangeFlags


or_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_e z80_main z80_flags =
    -- case 0xB3: or(E); break;
    --z80 |> set_flag_regs (z80_or z80.main.e z80.flags)
    z80_flags |> z80_or z80_main.e |> Z80ChangeFlags


or_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_h z80_main z80_flags =
    -- case 0xB4: or(HL>>>8); break;
    -- case 0xB4: or(xy>>>8); break;
    --z80 |> set_flag_regs (z80_or (get_h ixiyhl z80.main) z80.flags)
    z80_flags |> z80_or (shiftRightBy8 z80_main.hl) |> Z80ChangeFlags


or_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_l z80_main z80_flags =
    -- case 0xB5: or(HL&0xFF); break;
    -- case 0xB5: or(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_or (get_l ixiyhl z80.main) z80.flags)
    z80_flags |> z80_or (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


cp_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_b z80_main z80_flags =
    -- case 0xB8: cp(B); break;
    --z80 |> set_flag_regs (cp z80.main.b z80.flags)
    z80_flags |> z80_cp z80_main.b |> Z80ChangeFlags


cp_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_c z80_main z80_flags =
    -- case 0xB9: cp(C); break;
    --z80 |> set_flag_regs (cp z80.main.c z80.flags)
    z80_flags |> z80_cp z80_main.c |> Z80ChangeFlags


cp_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_d z80_main z80_flags =
    -- case 0xBA: cp(D); break;
    --z80 |> set_flag_regs (cp z80.main.d z80.flags)
    z80_flags |> z80_cp z80_main.d |> Z80ChangeFlags


cp_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_e z80_main z80_flags =
    -- case 0xBB: cp(E); break;
    --z80 |> set_flag_regs (cp z80.main.e z80.flags)
    z80_flags |> z80_cp z80_main.e |> Z80ChangeFlags


cp_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_h z80_main z80_flags =
    -- case 0xBC: cp(HL>>>8); break;
    -- case 0xBC: cp(xy>>>8); break;
    --z80 |> set_flag_regs (cp (get_h ixiyhl z80.main) z80.flags)
    z80_flags |> z80_cp (shiftRightBy8 z80_main.hl) |> Z80ChangeFlags


cp_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_l z80_main z80_flags =
    -- case 0xBD: cp(HL&0xFF); break;
    -- case 0xBD: cp(xy&0xFF); break;
    --z80 |> set_flag_regs (cp (get_l ixiyhl z80.main) z80.flags)
    z80_flags |> z80_cp (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags
