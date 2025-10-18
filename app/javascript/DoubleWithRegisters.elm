module DoubleWithRegisters exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import Utils exposing (byte, shiftLeftBy8)
import Z80Core exposing (CoreChange(..), Z80Core)
import Z80Flags exposing (FlagRegisters, adc, dec, inc, sbc, z80_add, z80_and, z80_cp, z80_or, z80_sub, z80_xor)
import Z80Mem exposing (getMem8)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters, set_b, set_c, set_d, set_e, set_h, set_l)


type DoubleWithRegisterChange
    = NewARegisterIndirect (MainWithIndexRegisters -> Int) Int
    | SetARegisterIndirect (MainWithIndexRegisters -> Int) Int
    | IndexedIndirectIncrement (MainWithIndexRegisters -> Int) Int
    | IndexedIndirectDecrement (MainWithIndexRegisters -> Int) Int
    | FlagOpIndexedIndirect (Int -> FlagRegisters -> FlagRegisters) (MainWithIndexRegisters -> Int) Int
    | NewRegisterIndirect (Int -> MainWithIndexRegisters -> MainWithIndexRegisters) (MainWithIndexRegisters -> Int) Int
    | RegStore8BitValue Int (MainWithIndexRegisters -> Int) (MainWithIndexRegisters -> Int)
    | NewIXHValue Int
    | NewIXLValue Int
    | NewIYHValue Int
    | NewIYLValue Int


doubleWithRegistersIX : Dict Int ( Int -> DoubleWithRegisterChange, InstructionDuration )
doubleWithRegistersIX =
    Dict.fromList
        [ ( 0x26, ( ld_ix_h_n, ElevenTStates ) )
        , ( 0x2E, ( ld_ix_l_n, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_ix, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_ix, TwentyThreeTStates ) )

        -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
        , ( 0x46, ( \param -> NewRegisterIndirect set_b .ix (byte param), SevenTStates ) )

        -- case 0x4E: C=env.mem(getd(xy)); time+=3; break;
        , ( 0x4E, ( \param -> NewRegisterIndirect set_c .ix (byte param), SevenTStates ) )

        --case 0x56: D=env.mem(getd(xy)); time+=3; break;
        , ( 0x56, ( \param -> NewRegisterIndirect set_d .ix (byte param), SevenTStates ) )

        --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
        , ( 0x5E, ( \param -> NewRegisterIndirect set_e .ix (byte param), SevenTStates ) )

        -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
        , ( 0x66, ( \param -> NewRegisterIndirect set_h .ix (byte param), NineteenTStates ) )

        -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
        , ( 0x6E, ( \param -> NewRegisterIndirect set_l .ix (byte param), NineteenTStates ) )
        , ( 0x86, ( \param -> FlagOpIndexedIndirect z80_add .ix param, NineteenTStates ) )
        , ( 0x8E, ( \param -> FlagOpIndexedIndirect adc .ix param, NineteenTStates ) )
        , ( 0x96, ( \param -> FlagOpIndexedIndirect z80_sub .ix param, NineteenTStates ) )
        , ( 0x9E, ( \param -> FlagOpIndexedIndirect sbc .ix param, NineteenTStates ) )
        , ( 0xA6, ( \param -> FlagOpIndexedIndirect z80_and .ix param, NineteenTStates ) )
        , ( 0xAE, ( \param -> FlagOpIndexedIndirect z80_xor .ix param, NineteenTStates ) )
        , ( 0xB6, ( \param -> FlagOpIndexedIndirect z80_or .ix param, NineteenTStates ) )
        , ( 0xBE, ( \param -> FlagOpIndexedIndirect z80_cp .ix param, NineteenTStates ) )
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
        , ( 0x46, ( \param -> NewRegisterIndirect set_b .iy (byte param), SevenTStates ) )

        -- case 0x4E: C=env.mem(getd(xy)); time+=3; break;
        , ( 0x4E, ( \param -> NewRegisterIndirect set_c .iy (byte param), SevenTStates ) )

        --case 0x56: D=env.mem(getd(xy)); time+=3; break;
        , ( 0x56, ( \param -> NewRegisterIndirect set_d .iy (byte param), SevenTStates ) )

        --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
        , ( 0x5E, ( \param -> NewRegisterIndirect set_e .iy (byte param), SevenTStates ) )

        -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
        , ( 0x66, ( \param -> NewRegisterIndirect set_h .iy (byte param), NineteenTStates ) )

        -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
        , ( 0x6E, ( \param -> NewRegisterIndirect set_l .iy (byte param), NineteenTStates ) )
        , ( 0x86, ( \param -> FlagOpIndexedIndirect z80_add .iy param, NineteenTStates ) )
        , ( 0x8E, ( \param -> FlagOpIndexedIndirect adc .iy param, NineteenTStates ) )
        , ( 0x96, ( \param -> FlagOpIndexedIndirect z80_sub .iy param, NineteenTStates ) )
        , ( 0x9E, ( \param -> FlagOpIndexedIndirect sbc .iy param, NineteenTStates ) )
        , ( 0xA6, ( \param -> FlagOpIndexedIndirect z80_and .iy param, NineteenTStates ) )
        , ( 0xAE, ( \param -> FlagOpIndexedIndirect z80_xor .iy param, NineteenTStates ) )
        , ( 0xB6, ( \param -> FlagOpIndexedIndirect z80_or .iy param, NineteenTStates ) )
        , ( 0xBE, ( \param -> FlagOpIndexedIndirect z80_cp .iy param, NineteenTStates ) )
        , ( 0x77, ( ld_indirect_iy_a, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_iy, NineteenTStates ) )
        ]


ld_ix_h_n : Int -> DoubleWithRegisterChange
ld_ix_h_n param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    --Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.ix 0xFF) |> NewIXRegisterValue
    NewIXHValue param


ld_iy_h_n : Int -> DoubleWithRegisterChange
ld_iy_h_n param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    --Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.iy 0xFF) |> NewIYRegisterValue
    NewIYHValue param


ld_ix_l_n : Int -> DoubleWithRegisterChange
ld_ix_l_n param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    --Bitwise.or param (Bitwise.and z80_main.ix 0xFF00) |> NewIXRegisterValue
    NewIXLValue param


ld_iy_l_n : Int -> DoubleWithRegisterChange
ld_iy_l_n param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    --Bitwise.or param (Bitwise.and z80_main.iy 0xFF00) |> NewIYRegisterValue
    NewIYLValue param


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


applyDoubleWithRegistersDelta : CpuTimeCTime -> DoubleWithRegisterChange -> Z80ROM -> Z80Core -> CoreChange
applyDoubleWithRegistersDelta cpu_time z80changeData rom48k z80 =
    case z80changeData of
        NewIXHValue param ->
            let
                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.ix 0xFF)
            in
            { z80_main | ix = new_hl } |> MainOnly

        NewIXLValue param ->
            let
                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or param (Bitwise.and z80_main.ix 0xFF00)
            in
            { z80_main | ix = new_hl } |> MainOnly

        NewIYHValue param ->
            let
                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.iy 0xFF)
            in
            { z80_main | iy = new_hl } |> MainOnly

        NewIYLValue param ->
            let
                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or param (Bitwise.and z80_main.iy 0xFF00)
            in
            { z80_main | iy = new_hl } |> MainOnly

        RegStore8BitValue offset address_f value_f ->
            let
                main =
                    z80.main

                address =
                    ((main |> address_f) + (offset |> byte)) |> Bitwise.and 0xFFFF
            in
            SetMem8 address (main |> value_f)

        NewRegisterIndirect changeOneRegister addr offset ->
            let
                main =
                    z80.main

                ( new_b, newTime ) =
                    z80.env |> getMem8 ((main |> addr) + offset |> Bitwise.and 0xFFFF) cpu_time rom48k

                new_main =
                    main |> changeOneRegister new_b
            in
            new_main |> MainOnly

        NewARegisterIndirect addr_f offset ->
            let
                flags =
                    z80.flags

                addr =
                    ((z80.main |> addr_f) + byte offset) |> Bitwise.and 0xFFFF

                ( new_a, newTime ) =
                    z80.env |> getMem8 addr cpu_time rom48k
            in
            { flags | a = new_a } |> FlagsOnly

        SetARegisterIndirect addr_f param ->
            let
                addr =
                    (z80.main |> addr_f) + byte param
            in
            SetMem8 addr z80.flags.a

        FlagOpIndexedIndirect flagFunc address_f offset ->
            let
                flags =
                    z80.flags

                address =
                    (z80.main |> address_f) + byte offset |> Bitwise.and 0xFFFF

                ( value, newTime ) =
                    z80.env |> getMem8 address cpu_time rom48k
            in
            flags |> flagFunc value |> FlagsOnly

        IndexedIndirectIncrement inAddr_f offset ->
            let
                base_addr =
                    (z80.main |> inAddr_f) + byte offset |> Bitwise.and 0xFFFF

                ramAddr =
                    base_addr - 0x4000
            in
            if ramAddr >= 0 then
                let
                    ( value, newTime ) =
                        z80.env |> getMem8 base_addr cpu_time rom48k

                    valueWithFlags =
                        z80.flags |> inc value
                in
                SetMem8Flags base_addr valueWithFlags

            else
                NoCore

        IndexedIndirectDecrement inAddr_f offset ->
            let
                base_addr =
                    (z80.main |> inAddr_f) + byte offset |> Bitwise.and 0xFFFF

                ramAddr =
                    base_addr - 0x4000
            in
            if ramAddr >= 0 then
                let
                    ( value, newTime ) =
                        z80.env |> getMem8 base_addr cpu_time rom48k

                    valueWithFlags =
                        z80.flags |> dec value
                in
                SetMem8Flags base_addr valueWithFlags

            else
                NoCore
