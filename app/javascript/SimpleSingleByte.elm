module SimpleSingleByte exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement(..), InstructionDuration(..))
import Dict exposing (Dict)
import RegisterChange exposing (ChangeOneRegister(..), RegisterChange(..), Shifter(..))
import Utils exposing (BitTest(..), shiftRightBy8)
import Z80Flags exposing (FlagFunc(..))
import Z80Types exposing (IXIYHL(..), MainRegisters, MainWithIndexRegisters, get_bc, get_de)


singleByteMainRegs : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegs =
    Dict.fromList
        [ ( 0x03, ( inc_bc, SixTStates ) )
        , ( 0x0B, ( dec_bc, SixTStates ) )
        , ( 0x13, ( inc_de, SixTStates ) )
        , ( 0x1B, ( dec_de, SixTStates ) )
        , ( 0x23, ( inc_hl, SixTStates ) )
        , ( 0x2B, ( dec_hl, SixTStates ) )
        , ( 0x34, ( inc_indirect_hl, ElevenTStates ) )
        , ( 0x35, ( dec_indirect_hl, ElevenTStates ) )
        , ( 0x41, ( ld_b_c, FourTStates ) )
        , ( 0x42, ( ld_b_d, FourTStates ) )
        , ( 0x43, ( ld_b_e, FourTStates ) )
        , ( 0x44, ( \z80_main -> ld_b_h z80_main.hl, FourTStates ) )
        , ( 0x45, ( ld_b_l, FourTStates ) )
        , ( 0x48, ( ld_c_b, FourTStates ) )
        , ( 0x4A, ( ld_c_d, FourTStates ) )
        , ( 0x4B, ( ld_c_e, FourTStates ) )
        , ( 0x4C, ( ld_c_h, FourTStates ) )
        , ( 0x4D, ( ld_c_l, FourTStates ) )
        , ( 0x50, ( ld_d_b, FourTStates ) )
        , ( 0x51, ( ld_d_c, FourTStates ) )
        , ( 0x53, ( ld_d_e, FourTStates ) )
        , ( 0x54, ( ld_d_h, FourTStates ) )
        , ( 0x55, ( ld_d_l, FourTStates ) )
        , ( 0x58, ( ld_e_b, FourTStates ) )
        , ( 0x59, ( ld_e_c, FourTStates ) )
        , ( 0x5A, ( ld_e_d, FourTStates ) )
        , ( 0x5C, ( ld_e_h, FourTStates ) )
        , ( 0x5D, ( ld_e_l, FourTStates ) )
        , ( 0x60, ( ld_h_b, FourTStates ) )
        , ( 0x61, ( ld_h_c, FourTStates ) )
        , ( 0x62, ( ld_h_d, FourTStates ) )
        , ( 0x63, ( ld_h_e, FourTStates ) )
        , ( 0x65, ( ld_h_l, FourTStates ) )
        , ( 0x68, ( ld_l_b, FourTStates ) )
        , ( 0x69, ( ld_l_c, FourTStates ) )
        , ( 0x6A, ( ld_l_d, FourTStates ) )
        , ( 0x6B, ( ld_l_e, FourTStates ) )
        , ( 0x6C, ( ld_l_h, FourTStates ) )
        , ( 0x70, ( ld_indirect_hl_b, SevenTStates ) )
        , ( 0x71, ( ld_indirect_hl_c, SevenTStates ) )
        , ( 0x72, ( ld_indirect_hl_d, SevenTStates ) )
        , ( 0x73, ( ld_indirect_hl_e, SevenTStates ) )
        , ( 0x74, ( ld_indirect_hl_h, SevenTStates ) )
        , ( 0x75, ( ld_indirect_hl_l, SevenTStates ) )
        , ( 0x78, ( ld_a_b, FourTStates ) )
        , ( 0x79, ( ld_a_c, FourTStates ) )
        , ( 0x7A, ( ld_a_d, FourTStates ) )
        , ( 0x7B, ( ld_a_e, FourTStates ) )
        , ( 0x7C, ( ld_a_h, FourTStates ) )
        , ( 0x7D, ( ld_a_l, FourTStates ) )
        , ( 0xC5, ( push_bc, ElevenTStates ) )
        , ( 0xD5, ( push_de, ElevenTStates ) )
        , ( 0xE3, ( ex_indirect_sp_hl, NineteenTStates ) )
        , ( 0xE5, ( \z80_main -> PushedValue z80_main.hl, ElevenTStates ) )
        , ( 0xE9, ( jp_hl, FourTStates ) )
        , ( 0xEB, ( ex_de_hl, FourTStates ) )

        -- case 0xF9: SP=HL; time+=2; break;
        , ( 0xF9, ( \z80_main -> RegChangeNewSP z80_main.hl, SixTStates ) )
        ]


commonDDFDOps : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
commonDDFDOps =
    Dict.fromList
        [ ( 0x40, ( \_ -> RegChangeNoOp, EightTStates ) )
        , ( 0x41, ( ld_b_c, EightTStates ) )
        , ( 0x42, ( ld_b_d, EightTStates ) )
        , ( 0x43, ( ld_b_e, EightTStates ) )
        , ( 0x48, ( ld_c_b, EightTStates ) )
        , ( 0x49, ( \_ -> RegChangeNoOp, EightTStates ) )
        , ( 0x4A, ( ld_c_d, EightTStates ) )
        , ( 0x4B, ( ld_c_e, EightTStates ) )
        , ( 0x50, ( ld_d_b, EightTStates ) )
        , ( 0x51, ( ld_d_c, EightTStates ) )
        , ( 0x52, ( \_ -> RegChangeNoOp, EightTStates ) )
        , ( 0x53, ( ld_d_e, EightTStates ) )
        , ( 0x58, ( ld_e_b, EightTStates ) )
        , ( 0x59, ( ld_e_c, EightTStates ) )
        , ( 0x5A, ( ld_e_d, EightTStates ) )
        , ( 0x5B, ( \_ -> RegChangeNoOp, EightTStates ) )
        , ( 0x78, ( ld_a_b, EightTStates ) )
        , ( 0x79, ( ld_a_c, EightTStates ) )
        , ( 0x7A, ( ld_a_d, EightTStates ) )
        , ( 0x7B, ( ld_a_e, EightTStates ) )

        -- FD 6D is LD IYL, IYL
        , ( 0x64, ( \_ -> RegChangeNoOp, EightTStates ) )
        , ( 0x6D, ( \_ -> RegChangeNoOp, EightTStates ) )

        -- DD 7F and FD 7F are both No-ops
        , ( 0x7F, ( \_ -> RegChangeNoOp, EightTStates ) )
        , ( 0xEB, ( ex_de_hl, FourTStates ) )
        ]


singleByteMainRegsFD : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsFD =
    Dict.fromList
        [ ( 0x23, ( inc_iy, TenTStates ) )
        , ( 0x2B, ( dec_iy, TenTStates ) )
        , ( 0x44, ( \z80_main -> ld_b_h z80_main.iy, FourTStates ) )
        , ( 0x45, ( ld_b_iyl, EightTStates ) )
        , ( 0x4C, ( ld_c_iyh, EightTStates ) )
        , ( 0x4D, ( ld_c_iyl, EightTStates ) )
        , ( 0x54, ( ld_d_iy_h, EightTStates ) )
        , ( 0x55, ( ld_d_iy_l, EightTStates ) )
        , ( 0x5C, ( ld_e_iy_h, EightTStates ) )
        , ( 0x5D, ( ld_e_iy_l, EightTStates ) )
        , ( 0x60, ( \z80_main -> ChangeRegisterIYH z80_main.b, EightTStates ) )
        , ( 0x61, ( \z80_main -> ChangeRegisterIYH z80_main.c, EightTStates ) )
        , ( 0x62, ( \z80_main -> ChangeRegisterIYH z80_main.d, EightTStates ) )
        , ( 0x63, ( \z80_main -> ChangeRegisterIYH z80_main.e, EightTStates ) )
        , ( 0x65, ( \z80_main -> ChangeRegisterIYH (Bitwise.and z80_main.iy 0xFF), EightTStates ) )
        , ( 0x68, ( \z80_main -> ChangeRegisterIYL z80_main.b, EightTStates ) )
        , ( 0x69, ( \z80_main -> ChangeRegisterIYL z80_main.c, EightTStates ) )
        , ( 0x6A, ( \z80_main -> ChangeRegisterIYL z80_main.d, EightTStates ) )
        , ( 0x6B, ( \z80_main -> ChangeRegisterIYL z80_main.e, EightTStates ) )
        , ( 0x6C, ( \z80_main -> ChangeRegisterIYL (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0x7C, ( \z80_main -> SingleRegisterChange AlterRegisterA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0x7D, ( \z80_main -> SingleRegisterChange AlterRegisterA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0x84, ( \z80_main -> SingleEnvFlagFunc AddA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0x85, ( \z80_main -> SingleEnvFlagFunc AddA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0x8C, ( \z80_main -> SingleEnvFlagFunc AdcA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0x8D, ( \z80_main -> SingleEnvFlagFunc AdcA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0x94, ( \z80_main -> SingleEnvFlagFunc SubA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0x95, ( \z80_main -> SingleEnvFlagFunc SubA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0x9C, ( \z80_main -> SingleEnvFlagFunc SbcA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0x9D, ( \z80_main -> SingleEnvFlagFunc SbcA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xA4, ( \z80_main -> SingleEnvFlagFunc AndA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0xA5, ( \z80_main -> SingleEnvFlagFunc AndA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xAC, ( \z80_main -> SingleEnvFlagFunc XorA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0xAD, ( \z80_main -> SingleEnvFlagFunc XorA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xB4, ( \z80_main -> SingleEnvFlagFunc OrA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0xB5, ( \z80_main -> SingleEnvFlagFunc OrA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xBC, ( \z80_main -> SingleEnvFlagFunc CpA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0xBD, ( \z80_main -> SingleEnvFlagFunc CpA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xE3, ( ex_indirect_sp_iy, TwentyThreeTStates ) )
        , ( 0xE5, ( \z80_main -> PushedValue z80_main.iy, FifteenTStates ) )
        , ( 0xE9, ( jp_iy, EightTStates ) )

        -- case 0xF9: SP=xy; time+=2; break;
        , ( 0xF9, ( \z80_main -> RegChangeNewSP z80_main.iy, TenTStates ) )
        ]
        |> Dict.union commonDDFDOps


singleByteMainRegsDD : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsDD =
    Dict.fromList
        [ ( 0x23, ( inc_ix, TenTStates ) )
        , ( 0x2B, ( dec_ix, TenTStates ) )
        , ( 0x44, ( \z80_main -> ld_b_h z80_main.ix, FourTStates ) )
        , ( 0x45, ( ld_b_ixl, EightTStates ) )
        , ( 0x4C, ( ld_c_ixh, EightTStates ) )
        , ( 0x4D, ( ld_c_ixl, EightTStates ) )
        , ( 0x54, ( ld_d_ix_h, EightTStates ) )
        , ( 0x55, ( ld_d_ix_l, EightTStates ) )
        , ( 0x5C, ( ld_e_ix_h, EightTStates ) )
        , ( 0x5D, ( ld_e_ix_l, EightTStates ) )
        , ( 0x60, ( \z80_main -> ChangeRegisterIXH z80_main.b, EightTStates ) )
        , ( 0x61, ( \z80_main -> ChangeRegisterIXH z80_main.c, EightTStates ) )
        , ( 0x62, ( \z80_main -> ChangeRegisterIXH z80_main.d, EightTStates ) )
        , ( 0x63, ( \z80_main -> ChangeRegisterIXH z80_main.e, EightTStates ) )
        , ( 0x65, ( \z80_main -> ChangeRegisterIXH (Bitwise.and z80_main.ix 0xFF), EightTStates ) )
        , ( 0x68, ( \z80_main -> ChangeRegisterIXL z80_main.b, EightTStates ) )
        , ( 0x69, ( \z80_main -> ChangeRegisterIXL z80_main.c, EightTStates ) )
        , ( 0x6A, ( \z80_main -> ChangeRegisterIXL z80_main.d, EightTStates ) )
        , ( 0x6B, ( \z80_main -> ChangeRegisterIXL z80_main.e, EightTStates ) )
        , ( 0x6C, ( \z80_main -> ChangeRegisterIXL (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0x7C, ( \z80_main -> SingleRegisterChange AlterRegisterA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0x7D, ( \z80_main -> SingleRegisterChange AlterRegisterA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0x84, ( \z80_main -> SingleEnvFlagFunc AddA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0x85, ( \z80_main -> SingleEnvFlagFunc AddA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0x8C, ( \z80_main -> SingleEnvFlagFunc AdcA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0x8D, ( \z80_main -> SingleEnvFlagFunc AdcA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0x94, ( \z80_main -> SingleEnvFlagFunc SubA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0x95, ( \z80_main -> SingleEnvFlagFunc SubA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0x9C, ( \z80_main -> SingleEnvFlagFunc SbcA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0x9D, ( \z80_main -> SingleEnvFlagFunc SbcA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xA4, ( \z80_main -> SingleEnvFlagFunc AndA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0xA5, ( \z80_main -> SingleEnvFlagFunc AndA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xAC, ( \z80_main -> SingleEnvFlagFunc XorA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0xAD, ( \z80_main -> SingleEnvFlagFunc XorA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xB4, ( \z80_main -> SingleEnvFlagFunc OrA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0xB5, ( \z80_main -> SingleEnvFlagFunc OrA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xBC, ( \z80_main -> SingleEnvFlagFunc CpA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0xBD, ( \z80_main -> SingleEnvFlagFunc CpA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
        , ( 0xE3, ( ex_indirect_sp_ix, TwentyThreeTStates ) )
        , ( 0xE5, ( \z80_main -> PushedValue z80_main.ix, FifteenTStates ) )
        , ( 0xE9, ( jp_ix, EightTStates ) )

        -- case 0xF9: SP=xy; time+=2; break;
        , ( 0xF9, ( \z80_main -> RegChangeNewSP z80_main.ix, TenTStates ) )
        ]
        |> Dict.union commonDDFDOps


inc_bc : MainWithIndexRegisters -> RegisterChange
inc_bc z80_main =
    -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
    ----{ z80 | main = { z80_main | b = reg_b, c = reg_c } } |> add_cpu_time 2
    if z80_main.c == 0xFF then
        ChangeRegisterBC (Bitwise.and (z80_main.b + 1) 0xFF) 0

    else
        SingleRegisterChange AlterRegisterC (z80_main.c + 1)


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
        SingleRegisterChange AlterRegisterC tmp_c


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
        SingleRegisterChange ChangeRegisterE new_e


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
        SingleRegisterChange ChangeRegisterE new_e


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
    SingleRegisterChange AlterRegisterB z80_main.c


ld_b_d : MainWithIndexRegisters -> RegisterChange
ld_b_d z80_main =
    -- case 0x42: B=D; break;
    --z80 |> set_b z80.main.d
    SingleRegisterChange AlterRegisterB z80_main.d


ld_b_e : MainWithIndexRegisters -> RegisterChange
ld_b_e z80_main =
    -- case 0x43: B=E; break;
    --z80 |> set_b z80.main.e
    SingleRegisterChange AlterRegisterB z80_main.e


ld_b_h : Int -> RegisterChange
ld_b_h hl =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    SingleRegisterChange AlterRegisterB (shiftRightBy8 hl)


ld_c_ixh : MainWithIndexRegisters -> RegisterChange
ld_c_ixh z80_main =
    -- case 0x4C: C=xy>>>8; break;
    SingleRegisterChange AlterRegisterC (shiftRightBy8 z80_main.ix)


ld_c_iyh : MainWithIndexRegisters -> RegisterChange
ld_c_iyh z80_main =
    SingleRegisterChange AlterRegisterC (shiftRightBy8 z80_main.iy)


ld_b_l : MainWithIndexRegisters -> RegisterChange
ld_b_l z80_main =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    SingleRegisterChange AlterRegisterB (Bitwise.and z80_main.hl 0xFF)


ld_b_ixl : MainWithIndexRegisters -> RegisterChange
ld_b_ixl z80_main =
    -- case 0x45: B=xy&0xFF; break;
    SingleRegisterChange AlterRegisterB (Bitwise.and z80_main.ix 0xFF)


ld_b_iyl : MainWithIndexRegisters -> RegisterChange
ld_b_iyl z80_main =
    -- case 0x45: B=xy&0xFF; break;
    SingleRegisterChange AlterRegisterB (Bitwise.and z80_main.iy 0xFF)


ld_c_ixl : MainWithIndexRegisters -> RegisterChange
ld_c_ixl z80_main =
    -- case 0x4D: C=xy&0xFF; break;
    SingleRegisterChange AlterRegisterC (Bitwise.and z80_main.ix 0xFF)


ld_c_iyl : MainWithIndexRegisters -> RegisterChange
ld_c_iyl z80_main =
    -- case 0x4D: C=xy&0xFF; break;
    SingleRegisterChange AlterRegisterC (Bitwise.and z80_main.iy 0xFF)


ld_c_b : MainWithIndexRegisters -> RegisterChange
ld_c_b z80_main =
    -- case 0x48: C=B; break;
    --z80 |> set_c z80.main.b
    SingleRegisterChange AlterRegisterC z80_main.b


ld_c_d : MainWithIndexRegisters -> RegisterChange
ld_c_d z80_main =
    -- case 0x4A: C=D; break;
    --z80 |> set_c z80.main.d
    SingleRegisterChange AlterRegisterC z80_main.d


ld_c_e : MainWithIndexRegisters -> RegisterChange
ld_c_e z80_main =
    -- case 0x4B: C=E; break;
    --z80 |> set_c z80.main.e
    SingleRegisterChange AlterRegisterC z80_main.e


ld_c_h : MainWithIndexRegisters -> RegisterChange
ld_c_h z80_main =
    -- case 0x4C: C=HL>>>8; break;
    --z80 |> set_c (get_h ixiyhl z80.main)
    SingleRegisterChange AlterRegisterC (shiftRightBy8 z80_main.hl)


ld_c_l : MainWithIndexRegisters -> RegisterChange
ld_c_l z80_main =
    -- case 0x4D: C=HL&0xFF; break;
    --z80 |> set_c (get_l ixiyhl z80.main)
    SingleRegisterChange AlterRegisterC (Bitwise.and z80_main.hl 0xFF)


ld_d_b : MainWithIndexRegisters -> RegisterChange
ld_d_b z80_main =
    -- case 0x50: D=B; break;
    --z80 |> set_d z80.main.b
    SingleRegisterChange AlterRegisterD z80_main.b


ld_d_c : MainWithIndexRegisters -> RegisterChange
ld_d_c z80_main =
    -- case 0x51: D=C; break;
    --z80 |> set_d z80.main.c
    SingleRegisterChange AlterRegisterD z80_main.c


ld_d_e : MainWithIndexRegisters -> RegisterChange
ld_d_e z80_main =
    -- case 0x53: D=E; break;
    --z80 |> set_d z80.main.e
    SingleRegisterChange AlterRegisterD z80_main.e


ld_e_b : MainWithIndexRegisters -> RegisterChange
ld_e_b z80_main =
    -- case 0x58: E=B; break;
    --z80 |> set_e z80.main.b
    SingleRegisterChange ChangeRegisterE z80_main.b


ld_e_c : MainWithIndexRegisters -> RegisterChange
ld_e_c z80_main =
    -- case 0x59: E=C; break;
    --z80 |> set_e z80.main.c
    SingleRegisterChange ChangeRegisterE z80_main.c


ld_e_d : MainWithIndexRegisters -> RegisterChange
ld_e_d z80_main =
    -- case 0x5A: E=D; break;
    --z80 |> set_e z80.main.d
    SingleRegisterChange ChangeRegisterE z80_main.d


ld_e_h : MainWithIndexRegisters -> RegisterChange
ld_e_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    --z80 |> set_e (get_h ixiyhl z80.main)
    SingleRegisterChange ChangeRegisterE (shiftRightBy8 z80_main.hl)


ld_e_l : MainWithIndexRegisters -> RegisterChange
ld_e_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    --z80 |> set_e (get_l ixiyhl z80.main)
    SingleRegisterChange ChangeRegisterE (Bitwise.and z80_main.hl 0xFF)


ld_d_h : MainWithIndexRegisters -> RegisterChange
ld_d_h z80_main =
    -- case 0x54: D=HL>>>8; break;
    --z80 |> set_d (get_h ixiyhl z80.main)
    SingleRegisterChange AlterRegisterD (shiftRightBy8 z80_main.hl)


ld_d_l : MainWithIndexRegisters -> RegisterChange
ld_d_l z80_main =
    -- case 0x55: D=HL&0xFF; break;
    --z80 |> set_d (get_l ixiyhl z80.main)
    SingleRegisterChange AlterRegisterD (Bitwise.and z80_main.hl 0xFF)


ld_h_b : MainWithIndexRegisters -> RegisterChange
ld_h_b z80_main =
    -- case 0x60: HL=HL&0xFF|B<<8; break;
    -- case 0x60: xy=xy&0xFF|B<<8; break;
    --z80 |> set_h_z80 z80.main.b ixiyhl
    SingleRegisterChange ChangeRegisterH z80_main.b


ld_h_c : MainWithIndexRegisters -> RegisterChange
ld_h_c z80_main =
    -- case 0x61: HL=HL&0xFF|C<<8; break;
    -- case 0x61: xy=xy&0xFF|C<<8; break;
    --z80 |> set_h_z80 z80.main.c ixiyhl
    SingleRegisterChange ChangeRegisterH z80_main.c


ld_h_d : MainWithIndexRegisters -> RegisterChange
ld_h_d z80_main =
    -- case 0x62: HL=HL&0xFF|D<<8; break;
    -- case 0x62: xy=xy&0xFF|D<<8; break;
    --z80 |> set_h_z80 z80.main.d ixiyhl
    SingleRegisterChange ChangeRegisterH z80_main.d


ld_h_e : MainWithIndexRegisters -> RegisterChange
ld_h_e z80_main =
    -- case 0x63: HL=HL&0xFF|E<<8; break;
    -- case 0x63: xy=xy&0xFF|E<<8; break;
    --z80 |> set_h_z80 z80.main.e ixiyhl
    SingleRegisterChange ChangeRegisterH z80_main.e


ld_h_l : MainWithIndexRegisters -> RegisterChange
ld_h_l z80_main =
    -- case 0x65: HL=HL&0xFF|(HL&0xFF)<<8; break;
    -- case 0x65: xy=xy&0xFF|(xy&0xFF)<<8; break;
    --z80 |> set_h_z80 (get_l ixiyhl z80.main) ixiyhl
    SingleRegisterChange ChangeRegisterH (Bitwise.and z80_main.hl 0xFF)


ld_l_b : MainWithIndexRegisters -> RegisterChange
ld_l_b z80_main =
    -- case 0x68: HL=HL&0xFF00|B; break;
    -- case 0x68: xy=xy&0xFF00|B; break;
    --z80 |> set_l_z80 z80.main.b ixiyhl
    SingleRegisterChange ChangeRegisterL z80_main.b


ld_l_c : MainWithIndexRegisters -> RegisterChange
ld_l_c z80_main =
    -- case 0x69: HL=HL&0xFF00|C; break;
    -- case 0x69: xy=xy&0xFF00|C; break;
    SingleRegisterChange ChangeRegisterL z80_main.c


ld_l_d : MainWithIndexRegisters -> RegisterChange
ld_l_d z80_main =
    -- case 0x6A: HL=HL&0xFF00|D; break;
    -- case 0x6A: xy=xy&0xFF00|D; break;
    SingleRegisterChange ChangeRegisterL z80_main.d


ld_l_e : MainWithIndexRegisters -> RegisterChange
ld_l_e z80_main =
    -- case 0x6B: HL=HL&0xFF00|E; break;
    -- case 0x6B: xy=xy&0xFF00|E; break;
    SingleRegisterChange ChangeRegisterL z80_main.e


ld_l_h : MainWithIndexRegisters -> RegisterChange
ld_l_h z80_main =
    -- case 0x6C: HL=HL&0xFF00|HL>>>8; break;
    -- case 0x6C: xy=xy&0xFF00|xy>>>8; break;
    SingleRegisterChange ChangeRegisterL (shiftRightBy8 z80_main.hl)


ld_a_b : MainWithIndexRegisters -> RegisterChange
ld_a_b z80_main =
    -- case 0x78: A=B; break;
    --z80 |> set_a z80.main.b
    SingleRegisterChange AlterRegisterA z80_main.b


ld_a_c : MainWithIndexRegisters -> RegisterChange
ld_a_c z80_main =
    -- case 0x79: A=C; break;
    --z80 |> set_a z80.main.c
    SingleRegisterChange AlterRegisterA z80_main.c


ld_a_d : MainWithIndexRegisters -> RegisterChange
ld_a_d z80_main =
    -- case 0x7A: A=D; break;
    --z80 |> set_a z80.main.d
    SingleRegisterChange AlterRegisterA z80_main.d


ld_a_e : MainWithIndexRegisters -> RegisterChange
ld_a_e z80_main =
    -- case 0x7B: A=E; break;
    --z80 |> set_a z80.main.e
    SingleRegisterChange AlterRegisterA z80_main.e


ld_a_h : MainWithIndexRegisters -> RegisterChange
ld_a_h z80_main =
    -- case 0x7C: A=HL>>>8; break;
    -- case 0x7C: A=xy>>>8; break;
    --z80 |> set_a (get_h ixiyhl z80.main)
    SingleRegisterChange AlterRegisterA (shiftRightBy8 z80_main.hl)


ld_a_l : MainWithIndexRegisters -> RegisterChange
ld_a_l z80_main =
    -- case 0x7D: A=HL&0xFF; break;
    -- case 0x7D: A=xy&0xFF; break;
    --z80 |> set_a (get_l ixiyhl z80.main)
    SingleRegisterChange AlterRegisterA (Bitwise.and z80_main.hl 0xFF)


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


jp_ix : MainWithIndexRegisters -> RegisterChange
jp_ix z80_main =
    -- case 0xE9: PC=HL; break;
    -- case 0xE9: PC=xy; break;
    RegisterChangeJump z80_main.ix


jp_iy : MainWithIndexRegisters -> RegisterChange
jp_iy z80_main =
    -- case 0xE9: PC=HL; break;
    -- case 0xE9: PC=xy; break;
    RegisterChangeJump z80_main.iy


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


ld_d_ix_h : MainWithIndexRegisters -> RegisterChange
ld_d_ix_h z80_main =
    --    -- case 0x54: D=HL>>>8; break;
    SingleRegisterChange AlterRegisterD (shiftRightBy8 z80_main.ix)


ld_d_ix_l : MainWithIndexRegisters -> RegisterChange
ld_d_ix_l z80_main =
    --    -- case 0x55: D=HL&0xFF; break;
    SingleRegisterChange AlterRegisterD (Bitwise.and z80_main.ix 0xFF)


ld_d_iy_l : MainWithIndexRegisters -> RegisterChange
ld_d_iy_l z80_main =
    --    -- case 0x55: D=HL&0xFF; break;
    SingleRegisterChange AlterRegisterD (Bitwise.and z80_main.iy 0xFF)


ld_d_iy_h : MainWithIndexRegisters -> RegisterChange
ld_d_iy_h z80_main =
    --    -- case 0x54: D=HL>>>8; break;
    SingleRegisterChange AlterRegisterD (shiftRightBy8 z80_main.iy)


ld_e_iy_l : MainWithIndexRegisters -> RegisterChange
ld_e_iy_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    SingleRegisterChange ChangeRegisterE (Bitwise.and z80_main.iy 0xFF)


ld_e_ix_h : MainWithIndexRegisters -> RegisterChange
ld_e_ix_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    SingleRegisterChange ChangeRegisterE (shiftRightBy8 z80_main.ix)


ld_e_ix_l : MainWithIndexRegisters -> RegisterChange
ld_e_ix_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    SingleRegisterChange ChangeRegisterE (Bitwise.and z80_main.ix 0xFF)


ld_e_iy_h : MainWithIndexRegisters -> RegisterChange
ld_e_iy_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    SingleRegisterChange ChangeRegisterE (shiftRightBy8 z80_main.iy)


ex_indirect_sp_hl : MainWithIndexRegisters -> RegisterChange
ex_indirect_sp_hl _ =
    -- case 0xE3: v=pop(); push(HL); MP=HL=v; time+=2; break;
    ExchangeTopOfStackWith HL


ex_indirect_sp_ix : MainWithIndexRegisters -> RegisterChange
ex_indirect_sp_ix _ =
    -- case 0xE3: v=pop(); push(xy); MP=xy=v; time+=2; break;
    ExchangeTopOfStackWith IX


ex_indirect_sp_iy : MainWithIndexRegisters -> RegisterChange
ex_indirect_sp_iy _ =
    -- case 0xE3: v=pop(); push(xy); MP=xy=v; time+=2; break;
    ExchangeTopOfStackWith IY
