module DoubleWithRegisters exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import RegisterChange exposing (DoubleWithRegisterChange(..))
import SingleWith8BitParameter exposing (JumpChange(..))
import Z80Flags exposing (FlagFunc(..))
import Z80Registers exposing (ChangeMainRegister(..))
import Z80Types exposing (MainWithIndexRegisters)


doubleWithRegisters : Dict Int ( Int -> JumpChange, InstructionDuration )
doubleWithRegisters =
    Dict.fromList
        [ ( 0x26, ( ld_h_n, SevenTStates ) )
        , ( 0x2E, ( ld_l_n, SevenTStates ) )
        , ( 0x36, ( ld_indirect_hl_n, TenTStates ) )
        ]


doubleWithRegistersIX : Dict Int ( Int -> DoubleWithRegisterChange, InstructionDuration )
doubleWithRegistersIX =
    Dict.fromList
        [ ( 0x26, ( ld_ix_h_n, ElevenTStates ) )
        , ( 0x2E, ( ld_ix_l_n, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_ix, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_ix, TwentyThreeTStates ) )

        -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
        , ( 0x46, ( \param -> NewRegisterIndirect ChangeMainB .ix param, SevenTStates ) )

        -- case 0x4E: C=env.mem(getd(xy)); time+=3; break;
        , ( 0x4E, ( \param -> NewRegisterIndirect ChangeMainC .ix param, SevenTStates ) )

        --case 0x56: D=env.mem(getd(xy)); time+=3; break;
        , ( 0x56, ( \param -> NewRegisterIndirect ChangeMainD .ix param, SevenTStates ) )

        --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
        , ( 0x5E, ( \param -> NewRegisterIndirect ChangeMainE .ix param, SevenTStates ) )

        -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
        , ( 0x66, ( \param -> NewRegisterIndirect ChangeMainH .ix param, NineteenTStates ) )

        -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
        , ( 0x6E, ( \param -> NewRegisterIndirect ChangeMainL .ix param, NineteenTStates ) )
        , ( 0x86, ( \param -> FlagOpIndexedIndirect AddA .ix param, NineteenTStates ) )
        , ( 0x8E, ( \param -> FlagOpIndexedIndirect AdcA .ix param, NineteenTStates ) )
        , ( 0x96, ( \param -> FlagOpIndexedIndirect SubA .ix param, NineteenTStates ) )
        , ( 0x9E, ( \param -> FlagOpIndexedIndirect SbcA .ix param, NineteenTStates ) )
        , ( 0xA6, ( \param -> FlagOpIndexedIndirect AndA .ix param, NineteenTStates ) )
        , ( 0xAE, ( \param -> FlagOpIndexedIndirect XorA .ix param, NineteenTStates ) )
        , ( 0xB6, ( \param -> FlagOpIndexedIndirect OrA .ix param, NineteenTStates ) )
        , ( 0xBE, ( \param -> FlagOpIndexedIndirect CpA .ix param, NineteenTStates ) )
        , ( 0x77, ( ld_indirect_ix_a, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_ix, NineteenTStates ) )
        ]


doubleWithRegistersIY : Dict Int ( Int -> DoubleWithRegisterChange, InstructionDuration )
doubleWithRegistersIY =
    Dict.fromList
        [ ( 0x26, ( ld_iy_h_n, ElevenTStates ) )
        , ( 0x2E, ( ld_iy_l_n, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_iy, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_iy, TwentyThreeTStates ) )

        -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
        , ( 0x46, ( \param -> NewRegisterIndirect ChangeMainB .iy param, SevenTStates ) )

        -- case 0x4E: C=env.mem(getd(xy)); time+=3; break;
        , ( 0x4E, ( \param -> NewRegisterIndirect ChangeMainC .iy param, SevenTStates ) )

        --case 0x56: D=env.mem(getd(xy)); time+=3; break;
        , ( 0x56, ( \param -> NewRegisterIndirect ChangeMainD .iy param, SevenTStates ) )

        --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
        , ( 0x5E, ( \param -> NewRegisterIndirect ChangeMainE .iy param, SevenTStates ) )

        -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
        , ( 0x66, ( \param -> NewRegisterIndirect ChangeMainH .iy param, NineteenTStates ) )

        -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
        , ( 0x6E, ( \param -> NewRegisterIndirect ChangeMainL .iy param, NineteenTStates ) )
        , ( 0x86, ( \param -> FlagOpIndexedIndirect AddA .iy param, NineteenTStates ) )
        , ( 0x8E, ( \param -> FlagOpIndexedIndirect AdcA .iy param, NineteenTStates ) )
        , ( 0x96, ( \param -> FlagOpIndexedIndirect SubA .iy param, NineteenTStates ) )
        , ( 0x9E, ( \param -> FlagOpIndexedIndirect SbcA .iy param, NineteenTStates ) )
        , ( 0xA6, ( \param -> FlagOpIndexedIndirect AndA .iy param, NineteenTStates ) )
        , ( 0xAE, ( \param -> FlagOpIndexedIndirect XorA .iy param, NineteenTStates ) )
        , ( 0xB6, ( \param -> FlagOpIndexedIndirect OrA .iy param, NineteenTStates ) )
        , ( 0xBE, ( \param -> FlagOpIndexedIndirect CpA .iy param, NineteenTStates ) )
        , ( 0x77, ( ld_indirect_iy_a, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_iy, NineteenTStates ) )
        ]


ld_h_n : Int -> JumpChange
ld_h_n param =
    -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
    SimpleNewHValue param


ld_ix_h_n : Int -> DoubleWithRegisterChange
ld_ix_h_n param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    NewIXHRegisterValue param


ld_iy_h_n : Int -> DoubleWithRegisterChange
ld_iy_h_n param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    NewIYHRegisterValue param


ld_l_n : Int -> JumpChange
ld_l_n param =
    -- case 0x2E: HL=HL&0xFF00|imm8(); break;
    SimpleNewLValue param


ld_ix_l_n : Int -> DoubleWithRegisterChange
ld_ix_l_n param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    NewIXLRegisterValue param


ld_iy_l_n : Int -> DoubleWithRegisterChange
ld_iy_l_n param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    NewIYLRegisterValue param


ld_indirect_ix_a : Int -> DoubleWithRegisterChange
ld_indirect_ix_a param =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    --SetARegisterIndirect (z80_main.ix + byte param)
    SetARegisterIndirect .ix param


ld_indirect_iy_a : Int -> DoubleWithRegisterChange
ld_indirect_iy_a param =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    --SetARegisterIndirect (z80_main.iy + byte param)
    SetARegisterIndirect .iy param


ld_a_indirect_ix : Int -> DoubleWithRegisterChange
ld_a_indirect_ix param =
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    NewARegisterIndirect .ix param


ld_a_indirect_iy : Int -> DoubleWithRegisterChange
ld_a_indirect_iy param =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    --let
    --    address =
    --        z80_main.iy + byte param
    --in
    NewARegisterIndirect .iy param


ld_indirect_hl_n : Int -> JumpChange
ld_indirect_hl_n param =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    RegChangeStoreIndirect .hl param


inc_indirect_ix : Int -> DoubleWithRegisterChange
inc_indirect_ix param =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement .ix param


inc_indirect_iy : Int -> DoubleWithRegisterChange
inc_indirect_iy param =
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement .iy param


dec_indirect_ix : Int -> DoubleWithRegisterChange
dec_indirect_ix param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement .ix param


dec_indirect_iy : Int -> DoubleWithRegisterChange
dec_indirect_iy param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement .iy param
