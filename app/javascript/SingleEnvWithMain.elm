module SingleEnvWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import Utils exposing (BitTest, shiftLeftBy8, shiftRightBy8)
import Z80Core exposing (CoreChange(..), RareCoreChange(..), Z80Core)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters, adc, add16, c_F53, sbc, testBit, z80_add, z80_and, z80_cp, z80_or, z80_sub, z80_xor)
import Z80Mem exposing (getMem8)
import Z80Registers exposing (ChangeMainRegister(..), CoreRegister(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), MainWithIndexRegisters, get_bc, get_de, set_xy)


type SingleEnvMainChange
    = IndirectBitTest BitTest Int
    | SingleEnvNewHL16BitAdd IXIYHL Int Int
    | NewSPValue Int
    | LoadAIndirect (MainWithIndexRegisters -> Int)
    | LoadRegisterIndirect ChangeMainRegister (MainWithIndexRegisters -> Int)
    | FlagFuncIndirect (Int -> FlagRegisters -> FlagRegisters) (MainWithIndexRegisters -> Int)


singleEnvMainRegs : Dict Int ( MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegs =
    Dict.fromList
        [ ( 0x0A, ( ld_a_indirect_bc, SevenTStates ) )
        , ( 0x1A, ( ld_a_indirect_de, SevenTStates ) )
        , ( 0x33, ( inc_sp, SixTStates ) )
        , ( 0x39, ( add_hl_sp, ElevenTStates ) )
        , ( 0x3B, ( dec_sp, SixTStates ) )
        , ( 0x46, ( ld_b_indirect_hl, SevenTStates ) )
        , ( 0x4E, ( ld_c_indirect_hl, SevenTStates ) )
        , ( 0x56, ( ld_d_indirect_hl, SevenTStates ) )
        , ( 0x5E, ( ld_e_indirect_hl, SevenTStates ) )
        , ( 0x66, ( ld_h_indirect_hl, SevenTStates ) )
        , ( 0x6E, ( ld_l_indirect_hl, SevenTStates ) )
        , ( 0x7E, ( ld_a_indirect_hl, SevenTStates ) )
        , ( 0x86, ( add_a_indirect_hl, SevenTStates ) )
        , ( 0x8E, ( adc_a_indirect_hl, SevenTStates ) )
        , ( 0x96, ( sub_indirect_hl, SevenTStates ) )
        , ( 0x9E, ( sbc_indirect_hl, SevenTStates ) )
        , ( 0xA6, ( and_indirect_hl, SevenTStates ) )
        , ( 0xAE, ( xor_indirect_hl, SevenTStates ) )
        , ( 0xB6, ( or_indirect_hl, SevenTStates ) )
        , ( 0xBE, ( cp_indirect_hl, SevenTStates ) )
        ]


singleEnvMainRegsIX : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsIX =
    Dict.fromList
        [ ( 0x39, ( add_ix_sp, FifteenTStates ) )
        ]


singleEnvMainRegsIY : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsIY =
    Dict.fromList
        [ ( 0x39, ( add_iy_sp, FifteenTStates ) )
        ]


applySingleEnvMainChange : CpuTimeCTime -> SingleEnvMainChange -> Z80ROM -> Z80Core -> CoreChange
applySingleEnvMainChange clockTime z80changeData rom48k z80 =
    case z80changeData of
        NewSPValue int ->
            SetStackPointer int |> RareChange

        IndirectBitTest bitTest mp_address ->
            -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
            let
                ( value, newTime ) =
                    z80.env |> getMem8 mp_address clockTime rom48k

                new_flags =
                    z80.flags |> testBit bitTest value
            in
            { new_flags
                | ff = new_flags.ff |> Bitwise.and (Bitwise.complement c_F53) |> Bitwise.or (mp_address |> shiftRightBy8 |> Bitwise.and c_F53)
            }
                |> FlagsOnly

        SingleEnvNewHL16BitAdd ixiyhl hl sp ->
            let
                new_xy =
                    add16 hl sp z80.flags
            in
            ChangeMainAndFlags (z80.main |> set_xy new_xy.value ixiyhl) new_xy.flags

        LoadAIndirect function ->
            let
                v =
                    z80.main |> function

                flags =
                    z80.flags

                ( value, newTime ) =
                    z80.env |> getMem8 v clockTime rom48k
            in
            { flags | a = value } |> FlagsOnly

        LoadRegisterIndirect changeMainRegister function ->
            let
                main =
                    z80.main

                address =
                    main |> function

                ( value, newTime ) =
                    z80.env |> getMem8 address clockTime rom48k
            in
            case changeMainRegister of
                ChangeMainB ->
                    { main | b = value } |> MainOnly

                ChangeMainC ->
                    { main | c = value } |> MainOnly

                ChangeMainD ->
                    { main | d = value } |> MainOnly

                ChangeMainE ->
                    { main | e = value } |> MainOnly

                ChangeMainH ->
                    let
                        new_hl =
                            (main.hl |> Bitwise.and 0xFF) |> Bitwise.or (value |> shiftLeftBy8)
                    in
                    { main | hl = new_hl } |> MainOnly

                ChangeMainL ->
                    let
                        new_hl =
                            main.hl |> Bitwise.and 0xFF00 |> Bitwise.or value
                    in
                    { main | hl = new_hl } |> MainOnly

        FlagFuncIndirect flagFunc function ->
            let
                address =
                    z80.main |> function

                ( value, newTime ) =
                    z80.env |> getMem8 address clockTime rom48k

                flags =
                    z80.flags
            in
            flags |> flagFunc value |> FlagsOnly


ld_a_indirect_bc : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_a_indirect_bc z80_main rom48k clockTime z80_env =
    -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
    LoadAIndirect get_bc


ld_a_indirect_de : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_a_indirect_de z80_main rom48k clockTime z80_env =
    -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
    LoadAIndirect get_de


ld_b_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_b_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x46: B=env.mem(HL); time+=3; break;
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    LoadRegisterIndirect ChangeMainB .hl


ld_c_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_c_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x4E: C=env.mem(HL); time+=3; break;
    LoadRegisterIndirect ChangeMainC .hl


ld_d_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_d_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    LoadRegisterIndirect ChangeMainD .hl


ld_e_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_e_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x5E: E=env.mem(HL); time+=3; break;
    LoadRegisterIndirect ChangeMainE .hl


ld_h_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_h_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x66: HL=HL&0xFF|env.mem(HL)<<8; time+=3; break;
    -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
    LoadRegisterIndirect ChangeMainH .hl


ld_l_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_l_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x6E: HL=HL&0xFF00|env.mem(HL); time+=3; break;
    -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
    LoadRegisterIndirect ChangeMainL .hl


ld_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_a_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    LoadAIndirect .hl


add_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
add_a_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x86: add(env.mem(HL)); time+=3; break;
    FlagFuncIndirect z80_add .hl


adc_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
adc_a_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x8E: adc(env.mem(HL)); time+=3; break;
    FlagFuncIndirect adc .hl


sub_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
sub_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x96: sub(env.mem(HL)); time+=3; break;
    FlagFuncIndirect z80_sub .hl


sbc_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
sbc_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    FlagFuncIndirect sbc .hl


and_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
and_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    FlagFuncIndirect z80_and .hl


xor_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
xor_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    FlagFuncIndirect z80_xor .hl


or_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
or_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    FlagFuncIndirect z80_or .hl


cp_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
cp_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    FlagFuncIndirect z80_cp .hl


add_hl_sp : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
add_hl_sp z80_main rom48k clockTime z80_env =
    --case 0x39: HL=add16(HL,SP); break;
    SingleEnvNewHL16BitAdd HL z80_main.hl z80_env.sp


add_ix_sp : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
add_ix_sp z80_main rom48k z80_env =
    --case 0x39: xy=add16(xy,SP); break;
    SingleEnvNewHL16BitAdd IX z80_main.ix z80_env.sp


add_iy_sp : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
add_iy_sp z80_main rom48k z80_env =
    --case 0x39: xy=add16(xy,SP); break;
    SingleEnvNewHL16BitAdd IY z80_main.iy z80_env.sp


inc_sp : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
inc_sp z80_main rom48k clockTime z80_env =
    -- case 0x33: SP=(char)(SP+1); time+=2; break;
    NewSPValue (Bitwise.and (z80_env.sp + 1) 0xFFFF)


dec_sp : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
dec_sp z80_main rom48k clockTime z80_env =
    -- case 0x3B: SP=(char)(SP-1); time+=2; break;
    NewSPValue (Bitwise.and (z80_env.sp - 1) 0xFFFF)
