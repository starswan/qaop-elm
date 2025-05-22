module SimpleSingleByte exposing (..)

import Bitwise exposing (complement)
import CpuTimeCTime exposing (CpuTimeIncrement(..), InstructionDuration(..))
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..))
import RegisterChange exposing (RegisterChange(..), Shifter(..))
import Utils exposing (BitTest(..), bitMaskFromBit, shiftRightBy8)
import Z80Types exposing (IXIYHL(..), MainRegisters, MainWithIndexRegisters, get_bc, get_de)


singleByteMainRegs : Dict Int ( MainWithIndexRegisters -> RegisterChange, PCIncrement, InstructionDuration )
singleByteMainRegs =
    Dict.fromList
        [ ( 0x03, ( inc_bc, IncrementByOne, SixTStates ) )
        , ( 0x0B, ( dec_bc, IncrementByOne, SixTStates ) )
        , ( 0x13, ( inc_de, IncrementByOne, SixTStates ) )
        , ( 0x1B, ( dec_de, IncrementByOne, SixTStates ) )
        , ( 0x23, ( inc_hl, IncrementByOne, SixTStates ) )
        , ( 0xDD23, ( inc_ix, IncrementByTwo, TenTStates ) )
        , ( 0xFD23, ( inc_iy, IncrementByTwo, TenTStates ) )
        , ( 0x2B, ( dec_hl, IncrementByOne, SixTStates ) )
        , ( 0xDD2B, ( dec_ix, IncrementByTwo, TenTStates ) )
        , ( 0xFD2B, ( dec_iy, IncrementByTwo, TenTStates ) )
        , ( 0x34, ( inc_indirect_hl, IncrementByOne, ElevenTStates ) )
        , ( 0x35, ( dec_indirect_hl, IncrementByOne, ElevenTStates ) )
        , ( 0x41, ( ld_b_c, IncrementByOne, FourTStates ) )
        , ( 0x42, ( ld_b_d, IncrementByOne, FourTStates ) )
        , ( 0x43, ( ld_b_e, IncrementByOne, FourTStates ) )
        , ( 0x44, ( ld_b_h, IncrementByOne, FourTStates ) )
        , ( 0x45, ( ld_b_l, IncrementByOne, FourTStates ) )
        , ( 0x48, ( ld_c_b, IncrementByOne, FourTStates ) )
        , ( 0x4A, ( ld_c_d, IncrementByOne, FourTStates ) )
        , ( 0x4B, ( ld_c_e, IncrementByOne, FourTStates ) )
        , ( 0x4C, ( ld_c_h, IncrementByOne, FourTStates ) )
        , ( 0x4D, ( ld_c_l, IncrementByOne, FourTStates ) )
        , ( 0x50, ( ld_d_b, IncrementByOne, FourTStates ) )
        , ( 0x51, ( ld_d_c, IncrementByOne, FourTStates ) )
        , ( 0x53, ( ld_d_e, IncrementByOne, FourTStates ) )
        , ( 0x54, ( ld_d_h, IncrementByOne, FourTStates ) )
        , ( 0x55, ( ld_d_l, IncrementByOne, FourTStates ) )
        , ( 0x58, ( ld_e_b, IncrementByOne, FourTStates ) )
        , ( 0x59, ( ld_e_c, IncrementByOne, FourTStates ) )
        , ( 0x5A, ( ld_e_d, IncrementByOne, FourTStates ) )
        , ( 0x5C, ( ld_e_h, IncrementByOne, FourTStates ) )
        , ( 0x5D, ( ld_e_l, IncrementByOne, FourTStates ) )
        , ( 0x60, ( ld_h_b, IncrementByOne, FourTStates ) )
        , ( 0x61, ( ld_h_c, IncrementByOne, FourTStates ) )
        , ( 0x62, ( ld_h_d, IncrementByOne, FourTStates ) )
        , ( 0x63, ( ld_h_e, IncrementByOne, FourTStates ) )
        , ( 0x65, ( ld_h_l, IncrementByOne, FourTStates ) )
        , ( 0x68, ( ld_l_b, IncrementByOne, FourTStates ) )
        , ( 0x69, ( ld_l_c, IncrementByOne, FourTStates ) )
        , ( 0x6A, ( ld_l_d, IncrementByOne, FourTStates ) )
        , ( 0x6B, ( ld_l_e, IncrementByOne, FourTStates ) )
        , ( 0x6C, ( ld_l_h, IncrementByOne, FourTStates ) )
        , ( 0x70, ( ld_indirect_hl_b, IncrementByOne, SevenTStates ) )
        , ( 0x71, ( ld_indirect_hl_c, IncrementByOne, SevenTStates ) )
        , ( 0x72, ( ld_indirect_hl_d, IncrementByOne, SevenTStates ) )
        , ( 0x73, ( ld_indirect_hl_e, IncrementByOne, SevenTStates ) )
        , ( 0x74, ( ld_indirect_hl_h, IncrementByOne, SevenTStates ) )
        , ( 0x75, ( ld_indirect_hl_l, IncrementByOne, SevenTStates ) )
        , ( 0x78, ( ld_a_b, IncrementByOne, FourTStates ) )
        , ( 0x79, ( ld_a_c, IncrementByOne, FourTStates ) )
        , ( 0x7A, ( ld_a_d, IncrementByOne, FourTStates ) )
        , ( 0x7B, ( ld_a_e, IncrementByOne, FourTStates ) )
        , ( 0x7C, ( ld_a_h, IncrementByOne, FourTStates ) )
        , ( 0x7D, ( ld_a_l, IncrementByOne, FourTStates ) )
        , ( 0xC5, ( push_bc, IncrementByOne, ElevenTStates ) )
        , ( 0xD5, ( push_de, IncrementByOne, ElevenTStates ) )
        , ( 0xE5, ( push_hl, IncrementByOne, ElevenTStates ) )
        , ( 0xE9, ( jp_hl, IncrementByOne, FourTStates ) )
        , ( 0xEB, ( ex_de_hl, IncrementByOne, FourTStates ) )
        , ( 0xF9, ( ld_sp_hl, IncrementByOne, SixTStates ) )
        , ( 0xCB06, ( rlc_indirect_hl, IncrementByTwo, FifteenTStates ) )
        , ( 0xCB0E, ( rrc_indirect_hl, IncrementByTwo, FifteenTStates ) )
        , ( 0xCB16, ( rl_indirect_hl, IncrementByTwo, FifteenTStates ) )
        , ( 0xCB1E, ( rr_indirect_hl, IncrementByTwo, FifteenTStates ) )
        , ( 0xCB26, ( sla_indirect_hl, IncrementByTwo, FifteenTStates ) )
        , ( 0xCB2E, ( sra_indirect_hl, IncrementByTwo, FifteenTStates ) )
        , ( 0xCB36, ( sll_indirect_hl, IncrementByTwo, FifteenTStates ) )
        , ( 0xCB3E, ( srl_indirect_hl, IncrementByTwo, FifteenTStates ) )
        , ( 0xCB80, ( resetBbit Bit_0, IncrementByTwo, EightTStates ) )
        , ( 0xCB81, ( resetCbit Bit_0, IncrementByTwo, EightTStates ) )
        , ( 0xCB88, ( resetBbit Bit_1, IncrementByTwo, EightTStates ) )
        , ( 0xCB90, ( resetBbit Bit_2, IncrementByTwo, EightTStates ) )
        , ( 0xCB98, ( resetBbit Bit_3, IncrementByTwo, EightTStates ) )
        , ( 0xCBA0, ( resetBbit Bit_4, IncrementByTwo, EightTStates ) )
        , ( 0xCBA8, ( resetBbit Bit_5, IncrementByTwo, EightTStates ) )
        , ( 0xCBB0, ( resetBbit Bit_6, IncrementByTwo, EightTStates ) )
        , ( 0xCBB8, ( resetBbit Bit_7, IncrementByTwo, EightTStates ) )
        ]


inc_bc : MainWithIndexRegisters -> RegisterChange
inc_bc z80_main =
    -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
    ----{ z80 | main = { z80_main | b = reg_b, c = reg_c } } |> add_cpu_time 2
    if z80_main.c == 0xFF then
        ChangeRegisterBC (Bitwise.and (z80_main.b + 1) 0xFF) 0

    else
        ChangeRegisterC (z80_main.c + 1)


dec_bc : MainWithIndexRegisters -> RegisterChange
dec_bc z80_main =
    -- case 0x0B: if(--C<0) B=B-1&(C=0xFF); time+=2; break;
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c }} |> add_cpu_time 2
    let
        tmp_c =
            z80_main.c - 1
    in
    if tmp_c < 0 then
        ChangeRegisterBC (Bitwise.and (z80_main.b - 1) 0xFF) 0xFF

    else
        ChangeRegisterC tmp_c


inc_de : MainWithIndexRegisters -> RegisterChange
inc_de z80_main =
    -- case 0x13: if(++E==256) {D=D+1&0xFF;E=0;} time+=2; break;
    let
        new_e =
            z80_main.e + 1
    in
    if new_e == 256 then
        ChangeRegisterDE (Bitwise.and (z80_main.d + 1) 0xFF) 0

    else
        ChangeRegisterE new_e


dec_de : MainWithIndexRegisters -> RegisterChange
dec_de z80_main =
    -- case 0x1B: if(--E<0) D=D-1&(E=0xFF); time+=2; break;
    let
        new_e =
            z80_main.e - 1
    in
    if new_e < 0 then
        ChangeRegisterDE (Bitwise.and (z80_main.d - 1) 0xFF) 0xFF

    else
        ChangeRegisterE new_e


inc_hl : MainWithIndexRegisters -> RegisterChange
inc_hl z80_main =
    -- case 0x23: HL=(char)(HL+1); time+=2; break;
    ChangeRegisterHL (Bitwise.and (z80_main.hl + 1) 0xFFFF)


inc_ix : MainWithIndexRegisters -> RegisterChange
inc_ix z80_main =
    -- case 0x23: xy=(char)(xy+1); time+=2; break;
    ChangeRegisterIX (Bitwise.and (z80_main.ix + 1) 0xFFFF)


inc_iy : MainWithIndexRegisters -> RegisterChange
inc_iy z80_main =
    -- case 0x23: xy=(char)(xy+1); time+=2; break;
    ChangeRegisterIY (Bitwise.and (z80_main.iy + 1) 0xFFFF)


dec_hl : MainWithIndexRegisters -> RegisterChange
dec_hl z80_main =
    -- case 0x2B: HL=(char)(HL-1); time+=2; break;
    let
        new_xy =
            Bitwise.and (z80_main.hl - 1) 0xFFFF
    in
    ChangeRegisterHL new_xy


dec_ix : MainWithIndexRegisters -> RegisterChange
dec_ix z80_main =
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    let
        new_xy =
            Bitwise.and (z80_main.ix - 1) 0xFFFF
    in
    ChangeRegisterIX new_xy


dec_iy : MainWithIndexRegisters -> RegisterChange
dec_iy z80_main =
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    let
        new_xy =
            Bitwise.and (z80_main.iy - 1) 0xFFFF
    in
    ChangeRegisterIY new_xy


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


push_bc : MainWithIndexRegisters -> RegisterChange
push_bc z80_main =
    -- case 0xC5: push(B<<8|C); break;
    --z80 |> push (z80 |> get_bc)
    let
        bc =
            z80_main |> get_bc

        --pushed = z80.env |> z80_push bc
    in
    --{ z80 | env = pushed }
    PushedValue bc


push_de : MainWithIndexRegisters -> RegisterChange
push_de z80_main =
    -- case 0xD5: push(D<<8|E); break;
    --z80 |> push (z80 |> get_de)
    let
        de =
            z80_main |> get_de

        --pushed = z80.env |> z80_push de
    in
    --{ z80 | env = pushed }
    PushedValue de


push_hl : MainWithIndexRegisters -> RegisterChange
push_hl z80_main =
    -- case 0xE5: push(HL); break;
    -- case 0xE5: push(xy); break;
    --let
    --    pushed =
    --        z80.env |> z80_push (z80.main |> get_xy ixiyhl)
    --in
    ----{ z80 | env = pushed }
    PushedValue z80_main.hl


ld_sp_hl : MainWithIndexRegisters -> RegisterChange
ld_sp_hl z80_main =
    -- case 0xF9: SP=HL; time+=2; break;
    RegChangeNewSP z80_main.hl


inc_indirect_hl : MainWithIndexRegisters -> RegisterChange
inc_indirect_hl z80_main =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IncrementIndirect z80_main.hl


dec_indirect_hl : MainWithIndexRegisters -> RegisterChange
dec_indirect_hl z80_main =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    DecrementIndirect z80_main.hl


jp_hl : MainWithIndexRegisters -> RegisterChange
jp_hl z80_main =
    -- case 0xE9: PC=HL; break;
    -- case 0xE9: PC=xy; break;
    RegisterChangeJump z80_main.hl


ld_indirect_hl_b : MainWithIndexRegisters -> RegisterChange
ld_indirect_hl_b z80_main =
    -- case 0x70: env.mem(HL,B); time+=3; break;
    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
    SetIndirect z80_main.hl z80_main.b


ld_indirect_hl_c : MainWithIndexRegisters -> RegisterChange
ld_indirect_hl_c z80_main =
    -- case 0x71: env.mem(HL,C); time+=3; break;
    -- case 0x71: env.mem(getd(xy),C); time+=3; break;
    SetIndirect z80_main.hl z80_main.c


ld_indirect_hl_d : MainWithIndexRegisters -> RegisterChange
ld_indirect_hl_d z80_main =
    -- case 0x72: env.mem(HL,D); time+=3; break;
    -- case 0x72: env.mem(getd(xy),D); time+=3; break;
    SetIndirect z80_main.hl z80_main.d


ld_indirect_hl_e : MainWithIndexRegisters -> RegisterChange
ld_indirect_hl_e z80_main =
    -- case 0x73: env.mem(HL,E); time+=3; break;
    -- case 0x73: env.mem(getd(xy),E); time+=3; break;
    SetIndirect z80_main.hl z80_main.e


ld_indirect_hl_h : MainWithIndexRegisters -> RegisterChange
ld_indirect_hl_h z80_main =
    -- case 0x74: env.mem(HL,HL>>>8); time+=3; break;
    -- case 0x74: env.mem(getd(xy),HL>>>8); time+=3; break;
    SetIndirect z80_main.hl (z80_main.hl |> shiftRightBy8)


ld_indirect_hl_l : MainWithIndexRegisters -> RegisterChange
ld_indirect_hl_l z80_main =
    -- case 0x75: env.mem(HL,HL&0xFF); time+=3; break;
    -- case 0x75: env.mem(getd(xy),HL&0xFF); time+=3; break;
    SetIndirect z80_main.hl (z80_main.hl |> Bitwise.and 0xFF)


ex_de_hl : MainWithIndexRegisters -> RegisterChange
ex_de_hl z80_main =
    -- case 0xEB: v=HL; HL=D<<8|E; D=v>>>8; E=v&0xFF; break;
    ChangeRegisterDEAndHL z80_main.hl (z80_main |> get_de)


rlc_indirect_hl : MainWithIndexRegisters -> RegisterChange
rlc_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter0 z80_main.hl


rrc_indirect_hl : MainWithIndexRegisters -> RegisterChange
rrc_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter1 z80_main.hl


rl_indirect_hl : MainWithIndexRegisters -> RegisterChange
rl_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter2 z80_main.hl


rr_indirect_hl : MainWithIndexRegisters -> RegisterChange
rr_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter3 z80_main.hl


sla_indirect_hl : MainWithIndexRegisters -> RegisterChange
sla_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter4 z80_main.hl


sra_indirect_hl : MainWithIndexRegisters -> RegisterChange
sra_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter5 z80_main.hl


sll_indirect_hl : MainWithIndexRegisters -> RegisterChange
sll_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter6 z80_main.hl


srl_indirect_hl : MainWithIndexRegisters -> RegisterChange
srl_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter7 z80_main.hl


resetBbit : BitTest -> MainWithIndexRegisters -> RegisterChange
resetBbit bitMask z80_main =
    --Bitwise.and raw.value (1 |> shiftLeftBy o |> complement)
    -- case 0x80: B=B&~(1<<o); break;
    ChangeRegisterB (bitMask |> bitMaskFromBit |> complement |> Bitwise.and z80_main.b)


resetCbit : BitTest -> MainWithIndexRegisters -> RegisterChange
resetCbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    ChangeRegisterC (bitMask |> bitMaskFromBit |> complement |> Bitwise.and z80_main.c)
