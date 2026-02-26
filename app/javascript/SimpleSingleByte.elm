module SimpleSingleByte exposing (..)

import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..))
import Dict exposing (Dict)
import RegisterChange exposing (RegisterChange(..), Shifter(..))
import Utils exposing (BitTest(..), shiftLeftBy8, shiftRightBy8)
import Z80Flags exposing (FlagFunc(..))
import Z80Registers exposing (ChangeMainRegister(..), ChangeSingle(..))
import Z80Types exposing (IXIYHL(..), MainRegisters, MainWithIndexRegisters, get_bc, get_de, get_l, set_de_main)


singleByteMainRegs : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegs =
    Dict.fromList
        [ ( 0x03, ( \z80_main -> TransformMainRegisters inc_bc, SixTStates ) )
        , ( 0x0B, ( \z80_main -> TransformMainRegisters dec_bc, SixTStates ) )
        , ( 0x13, ( \z80_main -> TransformMainRegisters inc_de, SixTStates ) )
        , ( 0x1B, ( \z80_main -> TransformMainRegisters dec_de, SixTStates ) )
        , ( 0x23, ( \z80_main -> TransformMainRegisters inc_hl, SixTStates ) )
        , ( 0x2B, ( \z80_main -> TransformMainRegisters dec_hl, SixTStates ) )

        -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
        -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
        , ( 0x34, ( \z80_main -> IncrementIndirect .hl, ElevenTStates ) )

        -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
        -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
        , ( 0x35, ( \z80_main -> DecrementIndirect .hl, ElevenTStates ) )
        , ( 0x41, ( \z80_main -> TransformMainRegisters ld_b_c, FourTStates ) )
        , ( 0x42, ( \z80_main -> TransformMainRegisters ld_b_d, FourTStates ) )
        , ( 0x43, ( \z80_main -> TransformMainRegisters ld_b_e, FourTStates ) )
        , ( 0x44, ( \z80_main -> TransformMainRegisters (ld_b_h .hl), FourTStates ) )
        , ( 0x45, ( \z80_main -> TransformMainRegisters (ld_b_l .hl), FourTStates ) )
        , ( 0x48, ( \z80_main -> TransformMainRegisters ld_c_b, FourTStates ) )
        , ( 0x4A, ( \z80_main -> TransformMainRegisters ld_c_d, FourTStates ) )
        , ( 0x4B, ( \z80_main -> TransformMainRegisters ld_c_e, FourTStates ) )
        , ( 0x4C, ( \z80_main -> TransformMainRegisters (ld_c_h .hl), FourTStates ) )
        , ( 0x4D, ( \z80_main -> TransformMainRegisters (ld_c_l .hl), FourTStates ) )
        , ( 0x50, ( \z80_main -> TransformMainRegisters ld_d_b, FourTStates ) )
        , ( 0x51, ( \z80_main -> TransformMainRegisters ld_d_c, FourTStates ) )
        , ( 0x53, ( \z80_main -> TransformMainRegisters ld_d_e, FourTStates ) )
        , ( 0x54, ( \z80_main -> TransformMainRegisters (ld_d_h .hl), FourTStates ) )
        , ( 0x55, ( \z80_main -> TransformMainRegisters (ld_d_l .hl), FourTStates ) )
        , ( 0x58, ( \z80_main -> TransformMainRegisters ld_e_b, FourTStates ) )
        , ( 0x59, ( \z80_main -> TransformMainRegisters ld_e_c, FourTStates ) )
        , ( 0x5A, ( \z80_main -> TransformMainRegisters ld_e_d, FourTStates ) )
        , ( 0x5C, ( \z80_main -> TransformMainRegisters ld_e_h, FourTStates ) )
        , ( 0x5D, ( \z80_main -> TransformMainRegisters ld_e_l, FourTStates ) )
        , ( 0x60, ( \z80_main -> TransformMainRegisters (ld_h_b .b), FourTStates ) )

        -- case 0x61: HL=HL&0xFF|C<<8; break;
        -- case 0x61: xy=xy&0xFF|C<<8; break;
        , ( 0x61, ( \z80_main -> TransformMainRegisters (ld_h_b .c), FourTStates ) )

        -- case 0x62: HL=HL&0xFF|D<<8; break;
        -- case 0x62: xy=xy&0xFF|D<<8; break;
        , ( 0x62, ( \z80_main -> TransformMainRegisters (ld_h_b .d), FourTStates ) )

        -- case 0x63: HL=HL&0xFF|E<<8; break;
        -- case 0x63: xy=xy&0xFF|E<<8; break;
        , ( 0x63, ( \z80_main -> TransformMainRegisters (ld_h_b .e), FourTStates ) )
        , ( 0x65, ( \z80_main -> TransformMainRegisters (ld_h_l get_l), FourTStates ) )
        , ( 0x68, ( \z80_main -> TransformMainRegisters (ld_l_b .b), FourTStates ) )
        , ( 0x69, ( \z80_main -> TransformMainRegisters (ld_l_b .c), FourTStates ) )
        , ( 0x6A, ( \z80_main -> TransformMainRegisters (ld_l_b .d), FourTStates ) )
        , ( 0x6B, ( \z80_main -> TransformMainRegisters (ld_l_b .e), FourTStates ) )
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

        -- case 0xC5: push(B<<8|C); break;
        , ( 0xC5, ( \z80_main -> PushedValue get_bc, ElevenTStates ) )

        -- case 0xD5: push(D<<8|E); break;
        , ( 0xD5, ( \z80_main -> PushedValue get_de, ElevenTStates ) )

        -- case 0xE3: v=pop(); push(HL); MP=HL=v; time+=2; break;
        , ( 0xE3, ( \z80_main -> ExchangeTopOfStackWith HL, NineteenTStates ) )
        , ( 0xE5, ( \z80_main -> PushedValue .hl, ElevenTStates ) )
        , ( 0xE9, ( \z80_main -> RegisterChangeJump .hl, FourTStates ) )
        , ( 0xEB, ( \z80_main -> TransformMainRegisters ex_de_hl, FourTStates ) )

        -- case 0xF9: SP=HL; time+=2; break;
        , ( 0xF9, ( \z80_main -> RegChangeNewSP .hl, SixTStates ) )
        ]


commonDDFDOps : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
commonDDFDOps =
    Dict.fromList
        [ ( 0x40, ( \_ -> RegChangeNoOp, EightTStates ) )
        , ( 0x41, ( \z80_main -> TransformMainRegisters ld_b_c, EightTStates ) )
        , ( 0x42, ( \z80_main -> TransformMainRegisters ld_b_d, EightTStates ) )
        , ( 0x43, ( \z80_main -> TransformMainRegisters ld_b_e, EightTStates ) )
        , ( 0x48, ( \z80_main -> TransformMainRegisters ld_c_b, EightTStates ) )
        , ( 0x49, ( \_ -> RegChangeNoOp, EightTStates ) )
        , ( 0x4A, ( \z80_main -> TransformMainRegisters ld_c_d, EightTStates ) )
        , ( 0x4B, ( \z80_main -> TransformMainRegisters ld_c_e, EightTStates ) )
        , ( 0x50, ( \z80_main -> TransformMainRegisters ld_d_b, EightTStates ) )
        , ( 0x51, ( \z80_main -> TransformMainRegisters ld_d_c, EightTStates ) )
        , ( 0x52, ( \_ -> RegChangeNoOp, EightTStates ) )
        , ( 0x53, ( \z80_main -> TransformMainRegisters ld_d_e, EightTStates ) )
        , ( 0x58, ( \z80_main -> TransformMainRegisters ld_e_b, EightTStates ) )
        , ( 0x59, ( \z80_main -> TransformMainRegisters ld_e_c, EightTStates ) )
        , ( 0x5A, ( \z80_main -> TransformMainRegisters ld_e_d, EightTStates ) )
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
        , ( 0xEB, ( \z80_main -> TransformMainRegisters ex_de_hl, FourTStates ) )
        ]


singleByteMainRegsFD : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsFD =
    Dict.fromList
        [ ( 0x23, ( \z80_main -> TransformMainRegisters inc_iy, TenTStates ) )
        , ( 0x2B, ( \z80_main -> TransformMainRegisters dec_iy, TenTStates ) )
        , ( 0x44, ( \z80_main -> TransformMainRegisters (ld_b_h .iy), EightTStates ) )
        , ( 0x45, ( \z80_main -> TransformMainRegisters (ld_b_l .iy), EightTStates ) )
        , ( 0x4C, ( \z80_main -> TransformMainRegisters (ld_c_h .iy), EightTStates ) )
        , ( 0x4D, ( \z80_main -> TransformMainRegisters (ld_c_l .iy), EightTStates ) )
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
        , ( 0x7C, ( \z80_main -> RegisterChangeA (z80_main.iy |> shiftRightBy8), EightTStates ) )
        , ( 0x7D, ( \z80_main -> RegisterChangeA (z80_main.iy |> Bitwise.and 0xFF), EightTStates ) )
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
        , ( 0xE5, ( \z80_main -> PushedValue .iy, FifteenTStates ) )
        , ( 0xE9, ( \z80_main -> RegisterChangeJump .iy, EightTStates ) )

        -- case 0xF9: SP=xy; time+=2; break;
        , ( 0xF9, ( \z80_main -> RegChangeNewSP .iy, TenTStates ) )
        ]
        |> Dict.union commonDDFDOps


singleByteMainRegsDD : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsDD =
    Dict.fromList
        [ ( 0x23, ( \z80_main -> TransformMainRegisters inc_ix, TenTStates ) )
        , ( 0x2B, ( \z80_main -> TransformMainRegisters dec_ix, TenTStates ) )
        , ( 0x44, ( \z80_main -> TransformMainRegisters (ld_b_h .ix), EightTStates ) )
        , ( 0x45, ( \z80_main -> TransformMainRegisters (ld_b_l .ix), EightTStates ) )
        , ( 0x4C, ( \z80_main -> TransformMainRegisters (ld_c_h .ix), EightTStates ) )
        , ( 0x4D, ( \z80_main -> TransformMainRegisters (ld_c_l .ix), EightTStates ) )
        , ( 0x54, ( ld_d_ix_h, EightTStates ) )
        , ( 0x55, ( ld_d_ix_l, EightTStates ) )
        , ( 0x5C, ( ld_e_ix_h, EightTStates ) )
        , ( 0x5D, ( ld_e_ix_l, EightTStates ) )
        , ( 0x60, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_h main.b), EightTStates ) )
        , ( 0x61, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_h main.c), EightTStates ) )
        , ( 0x62, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_h main.d), EightTStates ) )
        , ( 0x63, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_h main.e), EightTStates ) )
        , ( 0x65, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_h (Bitwise.and main.ix 0xFF)), EightTStates ) )
        , ( 0x68, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_l main.b), EightTStates ) )
        , ( 0x69, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_l main.c), EightTStates ) )
        , ( 0x6A, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_l main.d), EightTStates ) )
        , ( 0x6B, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_l main.e), EightTStates ) )
        , ( 0x6C, ( \z80_main -> TransformMainRegisters (\main -> main |> set_ix_l (main.ix |> shiftRightBy8)), EightTStates ) )
        , ( 0x7C, ( \z80_main -> RegisterChangeA (z80_main.ix |> shiftRightBy8), EightTStates ) )
        , ( 0x7D, ( \z80_main -> RegisterChangeA (z80_main.ix |> Bitwise.and 0xFF), EightTStates ) )
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

        -- case 0xE3: v=pop(); push(xy); MP=xy=v; time+=2; break;
        , ( 0xE3, ( \z80_main -> ExchangeTopOfStackWith IX, TwentyThreeTStates ) )
        , ( 0xE5, ( \z80_main -> PushedValue .ix, FifteenTStates ) )
        , ( 0xE9, ( \z80_main -> RegisterChangeJump .ix, EightTStates ) )

        -- case 0xF9: SP=xy; time+=2; break;
        , ( 0xF9, ( \z80_main -> RegChangeNewSP .ix, TenTStates ) )
        ]
        |> Dict.union commonDDFDOps


set_ix_h : Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_ix_h int main =
    { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF) (int |> shiftLeftBy8) }


set_ix_l : Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_ix_l int main =
    { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF00) int }


inc_bc : MainWithIndexRegisters -> MainWithIndexRegisters
inc_bc z80_main =
    -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
    --if z80_main.c == 0xFF then
    --    ChangeRegisterBC (Bitwise.and (z80_main.b + 1) 0xFF) 0
    --
    --else
    --    SingleRegisterChange ChangeSingleC (z80_main.c + 1)
    if z80_main.c == 0xFF then
        { z80_main | b = Bitwise.and (z80_main.b + 1) 0xFF, c = 0 }

    else
        { z80_main | c = z80_main.c + 1 }


dec_bc : MainWithIndexRegisters -> MainWithIndexRegisters
dec_bc z80_main =
    -- case 0x0B: if(--C<0) B=B-1&(C=0xFF); time+=2; break;
    let
        tmp_c =
            z80_main.c - 1
    in
    if tmp_c < 0 then
        { z80_main | b = Bitwise.and (z80_main.b - 1) 0xFF, c = 0xFF }
        --ChangeRegisterBC (Bitwise.and (z80_main.b - 1) 0xFF) 0xFF

    else
        --SingleRegisterChange ChangeMainC tmp_c
        { z80_main | c = tmp_c }


inc_de : MainWithIndexRegisters -> MainWithIndexRegisters
inc_de z80_main =
    -- case 0x13: if(++E==256) {D=D+1&0xFF;E=0;} time+=2; break;
    let
        new_e =
            z80_main.e + 1
    in
    if new_e == 256 then
        --ChangeRegisterDE (Bitwise.and (z80_main.d + 1) 0xFF) 0
        { z80_main | d = Bitwise.and (z80_main.d + 1) 0xFF, e = 0 }

    else
        -- SingleRegisterChange ChangeSingleE new_e
        { z80_main | e = new_e }


dec_de : MainWithIndexRegisters -> MainWithIndexRegisters
dec_de z80_main =
    -- case 0x1B: if(--E<0) D=D-1&(E=0xFF); time+=2; break;
    let
        new_e =
            z80_main.e - 1
    in
    if new_e < 0 then
        --ChangeRegisterDE (Bitwise.and (z80_main.d - 1) 0xFF) 0xFF
        { z80_main | d = Bitwise.and (z80_main.d - 1) 0xFF, e = 0xFF }

    else
        -- SingleRegisterChange ChangeSingleE new_e
        { z80_main | e = new_e }


inc_hl : MainWithIndexRegisters -> MainWithIndexRegisters
inc_hl z80_main =
    -- case 0x23: HL=(char)(HL+1); time+=2; break;
    --ChangeRegisterHL HL (Bitwise.and (z80_main.hl + 1) 0xFFFF)
    { z80_main | hl = Bitwise.and (z80_main.hl + 1) 0xFFFF }


inc_ix : MainWithIndexRegisters -> MainWithIndexRegisters
inc_ix z80_main =
    -- case 0x23: xy=(char)(xy+1); time+=2; break;
    { z80_main | ix = Bitwise.and (z80_main.ix + 1) 0xFFFF }


inc_iy : MainWithIndexRegisters -> MainWithIndexRegisters
inc_iy z80_main =
    -- case 0x23: xy=(char)(xy+1); time+=2; break;
    --ChangeRegisterHL IY (Bitwise.and (z80_main.iy + 1) 0xFFFF)
    { z80_main | iy = Bitwise.and (z80_main.iy + 1) 0xFFFF }


dec_hl : MainWithIndexRegisters -> MainWithIndexRegisters
dec_hl z80_main =
    -- case 0x2B: HL=(char)(HL-1); time+=2; break;
    --ChangeRegisterHL HL (Bitwise.and (z80_main.hl - 1) 0xFFFF)
    { z80_main | hl = Bitwise.and (z80_main.hl - 1) 0xFFFF }


dec_ix : MainWithIndexRegisters -> MainWithIndexRegisters
dec_ix z80_main =
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    --ChangeRegisterHL IX (Bitwise.and (z80_main.ix - 1) 0xFFFF)
    { z80_main | ix = Bitwise.and (z80_main.ix - 1) 0xFFFF }


dec_iy : MainWithIndexRegisters -> MainWithIndexRegisters
dec_iy z80_main =
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    --ChangeRegisterHL IY (Bitwise.and (z80_main.iy - 1) 0xFFFF)
    { z80_main | iy = Bitwise.and (z80_main.iy - 1) 0xFFFF }


ld_b_c : MainWithIndexRegisters -> MainWithIndexRegisters
ld_b_c z80_main =
    -- case 0x41: B=C; break;
    -- SingleRegisterChange ChangeSingleB z80_main.c
    { z80_main | b = z80_main.c }


ld_b_d : MainWithIndexRegisters -> MainWithIndexRegisters
ld_b_d z80_main =
    -- case 0x42: B=D; break;
    -- SingleRegisterChange ChangeSingleB z80_main.d
    { z80_main | b = z80_main.d }


ld_b_e : MainWithIndexRegisters -> MainWithIndexRegisters
ld_b_e z80_main =
    -- case 0x43: B=E; break;
    -- SingleRegisterChange ChangeSingleB z80_main.e
    { z80_main | b = z80_main.e }


ld_b_h : (MainWithIndexRegisters -> Int) -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_b_h hlf z80_main =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    -- SingleRegisterChange ChangeSingleB (shiftRightBy8 hl)
    { z80_main | b = shiftRightBy8 (z80_main |> hlf) }



--ld_c_ixh : MainWithIndexRegisters -> RegisterChange
--ld_c_ixh z80_main =
--    -- case 0x4C: C=xy>>>8; break;
--    SingleRegisterChange ChangeSingleC (shiftRightBy8 z80_main.ix)
--
--
--ld_c_iyh : MainWithIndexRegisters -> RegisterChange
--ld_c_iyh z80_main =
--    SingleRegisterChange ChangeSingleC (shiftRightBy8 z80_main.iy)


ld_b_l : (MainWithIndexRegisters -> Int) -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_b_l hlf z80_main =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    -- SingleRegisterChange ChangeSingleB (Bitwise.and z80_main.hl 0xFF)
    { z80_main | b = Bitwise.and (z80_main |> hlf) 0xFF }



--ld_b_ixl : MainWithIndexRegisters -> RegisterChange
--ld_b_ixl z80_main =
--    -- case 0x45: B=xy&0xFF; break;
--     SingleRegisterChange ChangeSingleB (Bitwise.and z80_main.ix 0xFF)
--
--
--ld_b_iyl : MainWithIndexRegisters -> RegisterChange
--ld_b_iyl z80_main =
--    -- case 0x45: B=xy&0xFF; break;
--     SingleRegisterChange ChangeSingleB (Bitwise.and z80_main.iy 0xFF)
--ld_c_ixl : MainWithIndexRegisters -> RegisterChange
--ld_c_ixl z80_main =
--    -- case 0x4D: C=xy&0xFF; break;
--    SingleRegisterChange ChangeSingleC (Bitwise.and z80_main.ix 0xFF)
--
--
--ld_c_iyl : MainWithIndexRegisters -> RegisterChange
--ld_c_iyl z80_main =
--    -- case 0x4D: C=xy&0xFF; break;
--    SingleRegisterChange ChangeSingleC (Bitwise.and z80_main.iy 0xFF)


ld_c_b : MainWithIndexRegisters -> MainWithIndexRegisters
ld_c_b z80_main =
    -- case 0x48: C=B; break;
    --SingleRegisterChange ChangeMainC z80_main.b
    { z80_main | c = z80_main.b }


ld_c_d : MainWithIndexRegisters -> MainWithIndexRegisters
ld_c_d z80_main =
    -- case 0x4A: C=D; break;
    --SingleRegisterChange ChangeMainC z80_main.d
    { z80_main | c = z80_main.d }


ld_c_e : MainWithIndexRegisters -> MainWithIndexRegisters
ld_c_e z80_main =
    -- case 0x4B: C=E; break;
    --SingleRegisterChange ChangeMainC z80_main.e
    { z80_main | c = z80_main.e }


ld_c_h : (MainWithIndexRegisters -> Int) -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_c_h hlf z80_main =
    -- case 0x4C: C=HL>>>8; break;
    --SingleRegisterChange ChangeMainC (shiftRightBy8 z80_main.hl)
    { z80_main | c = shiftRightBy8 (z80_main |> hlf) }


ld_c_l : (MainWithIndexRegisters -> Int) -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_c_l hlf z80_main =
    -- case 0x4D: C=HL&0xFF; break;
    --SingleRegisterChange ChangeMainC (Bitwise.and z80_main.hl 0xFF)
    { z80_main | c = Bitwise.and (z80_main |> hlf) 0xFF }


ld_d_b : MainWithIndexRegisters -> MainWithIndexRegisters
ld_d_b z80_main =
    -- case 0x50: D=B; break;
    -- SingleRegisterChange ChangeSingleD z80_main.b
    { z80_main | d = z80_main.b }


ld_d_c : MainWithIndexRegisters -> MainWithIndexRegisters
ld_d_c z80_main =
    -- case 0x51: D=C; break;
    -- SingleRegisterChange ChangeSingleD z80_main.c
    { z80_main | d = z80_main.c }


ld_d_e : MainWithIndexRegisters -> MainWithIndexRegisters
ld_d_e z80_main =
    -- case 0x53: D=E; break;
    -- SingleRegisterChange ChangeSingleD z80_main.e
    { z80_main | d = z80_main.e }


ld_e_b : MainWithIndexRegisters -> MainWithIndexRegisters
ld_e_b z80_main =
    -- case 0x58: E=B; break;
    -- SingleRegisterChange ChangeSingleE z80_main.b
    { z80_main | e = z80_main.b }


ld_e_c : MainWithIndexRegisters -> MainWithIndexRegisters
ld_e_c z80_main =
    -- case 0x59: E=C; break;
    -- SingleRegisterChange ChangeSingleE z80_main.c
    { z80_main | e = z80_main.c }


ld_e_d : MainWithIndexRegisters -> MainWithIndexRegisters
ld_e_d z80_main =
    -- case 0x5A: E=D; break;
    -- SingleRegisterChange ChangeSingleE z80_main.d
    { z80_main | e = z80_main.d }


ld_e_h : MainWithIndexRegisters -> MainWithIndexRegisters
ld_e_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    -- SingleRegisterChange ChangeSingleE (shiftRightBy8 z80_main.hl)
    { z80_main | e = shiftRightBy8 z80_main.hl }


ld_e_l : MainWithIndexRegisters -> MainWithIndexRegisters
ld_e_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    -- SingleRegisterChange ChangeSingleE (Bitwise.and z80_main.hl 0xFF)
    { z80_main | e = Bitwise.and z80_main.hl 0xFF }


ld_d_h : (MainWithIndexRegisters -> Int) -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_d_h hlf z80_main =
    -- case 0x54: D=HL>>>8; break;
    -- SingleRegisterChange ChangeSingleD (shiftRightBy8 z80_main.hl)
    { z80_main | d = shiftRightBy8 (z80_main |> hlf) }


ld_d_l : (MainWithIndexRegisters -> Int) -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_d_l hlf z80_main =
    -- case 0x55: D=HL&0xFF; break;
    -- SingleRegisterChange ChangeSingleD (Bitwise.and z80_main.hl 0xFF)
    { z80_main | d = Bitwise.and (z80_main |> hlf) 0xFF }


ld_h_b : (MainWithIndexRegisters -> Int) -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_h_b b_func z80_main =
    -- case 0x60: HL=HL&0xFF|B<<8; break;
    -- case 0x60: xy=xy&0xFF|B<<8; break;
    -- SingleRegisterChange ChangeSingleH z80_main.b
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (z80_main |> b_func |> shiftLeftBy8) }


ld_h_l : (MainWithIndexRegisters -> Int) -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_h_l bfunc z80_main =
    -- case 0x65: HL=HL&0xFF|(HL&0xFF)<<8; break;
    -- case 0x65: xy=xy&0xFF|(xy&0xFF)<<8; break;
    --SingleRegisterChange ChangeSingleH (Bitwise.and z80_main.hl 0xFF)
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 (z80_main |> bfunc)) }


ld_l_b : (MainWithIndexRegisters -> Int) -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_l_b bfunc z80_main =
    -- case 0x68: HL=HL&0xFF00|B; break;
    -- case 0x68: xy=xy&0xFF00|B; break;
    --SingleRegisterChange ChangeSingleL z80_main.b
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) (z80_main |> bfunc) }


ld_l_c : MainWithIndexRegisters -> RegisterChange
ld_l_c z80_main =
    -- case 0x69: HL=HL&0xFF00|C; break;
    -- case 0x69: xy=xy&0xFF00|C; break;
    SingleRegisterChange ChangeSingleL z80_main.c


ld_l_d : MainWithIndexRegisters -> RegisterChange
ld_l_d z80_main =
    -- case 0x6A: HL=HL&0xFF00|D; break;
    -- case 0x6A: xy=xy&0xFF00|D; break;
    SingleRegisterChange ChangeSingleL z80_main.d


ld_l_e : MainWithIndexRegisters -> RegisterChange
ld_l_e z80_main =
    -- case 0x6B: HL=HL&0xFF00|E; break;
    -- case 0x6B: xy=xy&0xFF00|E; break;
    SingleRegisterChange ChangeSingleL z80_main.e


ld_l_h : MainWithIndexRegisters -> RegisterChange
ld_l_h z80_main =
    -- case 0x6C: HL=HL&0xFF00|HL>>>8; break;
    -- case 0x6C: xy=xy&0xFF00|xy>>>8; break;
    SingleRegisterChange ChangeSingleL (shiftRightBy8 z80_main.hl)


ld_a_b : MainWithIndexRegisters -> RegisterChange
ld_a_b z80_main =
    -- case 0x78: A=B; break;
    RegisterChangeA z80_main.b


ld_a_c : MainWithIndexRegisters -> RegisterChange
ld_a_c z80_main =
    -- case 0x79: A=C; break;
    RegisterChangeA z80_main.c


ld_a_d : MainWithIndexRegisters -> RegisterChange
ld_a_d z80_main =
    -- case 0x7A: A=D; break;
    RegisterChangeA z80_main.d


ld_a_e : MainWithIndexRegisters -> RegisterChange
ld_a_e z80_main =
    -- case 0x7B: A=E; break;
    RegisterChangeA z80_main.e


ld_a_h : MainWithIndexRegisters -> RegisterChange
ld_a_h z80_main =
    -- case 0x7C: A=HL>>>8; break;
    -- case 0x7C: A=xy>>>8; break;
    RegisterChangeA (shiftRightBy8 z80_main.hl)


ld_a_l : MainWithIndexRegisters -> RegisterChange
ld_a_l z80_main =
    -- case 0x7D: A=HL&0xFF; break;
    -- case 0x7D: A=xy&0xFF; break;
    RegisterChangeA (Bitwise.and z80_main.hl 0xFF)


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


ex_de_hl : MainWithIndexRegisters -> MainWithIndexRegisters
ex_de_hl z80_main =
    -- case 0xEB: v=HL; HL=D<<8|E; D=v>>>8; E=v&0xFF; break;
    --ChangeRegisterDEAndHL z80_main.hl (z80_main |> get_de)
    { z80_main | hl = z80_main |> get_de } |> set_de_main z80_main.hl


ld_d_ix_h : MainWithIndexRegisters -> RegisterChange
ld_d_ix_h z80_main =
    -- case 0x54: D=HL>>>8; break;
    SingleRegisterChange ChangeSingleD (shiftRightBy8 z80_main.ix)


ld_d_ix_l : MainWithIndexRegisters -> RegisterChange
ld_d_ix_l z80_main =
    -- case 0x55: D=HL&0xFF; break;
    SingleRegisterChange ChangeSingleD (Bitwise.and z80_main.ix 0xFF)


ld_d_iy_l : MainWithIndexRegisters -> RegisterChange
ld_d_iy_l z80_main =
    -- case 0x55: D=HL&0xFF; break;
    SingleRegisterChange ChangeSingleD (Bitwise.and z80_main.iy 0xFF)


ld_d_iy_h : MainWithIndexRegisters -> RegisterChange
ld_d_iy_h z80_main =
    -- case 0x54: D=HL>>>8; break;
    SingleRegisterChange ChangeSingleD (shiftRightBy8 z80_main.iy)


ld_e_iy_l : MainWithIndexRegisters -> RegisterChange
ld_e_iy_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    SingleRegisterChange ChangeSingleE (Bitwise.and z80_main.iy 0xFF)


ld_e_ix_h : MainWithIndexRegisters -> RegisterChange
ld_e_ix_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    SingleRegisterChange ChangeSingleE (shiftRightBy8 z80_main.ix)


ld_e_ix_l : MainWithIndexRegisters -> RegisterChange
ld_e_ix_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    SingleRegisterChange ChangeSingleE (Bitwise.and z80_main.ix 0xFF)


ld_e_iy_h : MainWithIndexRegisters -> RegisterChange
ld_e_iy_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    SingleRegisterChange ChangeSingleE (shiftRightBy8 z80_main.iy)


ex_indirect_sp_iy : MainWithIndexRegisters -> RegisterChange
ex_indirect_sp_iy _ =
    -- case 0xE3: v=pop(); push(xy); MP=xy=v; time+=2; break;
    ExchangeTopOfStackWith IY
