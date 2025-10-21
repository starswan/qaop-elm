module DoubleWithRegisters exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import PCIncrement exposing (MediumPCIncrement(..))
import Utils exposing (byte, shiftLeftBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (mem, setMem)
import Z80Flags exposing (FlagFunc(..), changeFlags, dec, inc)
import Z80Registers exposing (ChangeMainRegister(..), CoreRegister)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), MainWithIndexRegisters)


type DoubleWithRegisterChange
    = DoubleRegChangeStoreIndirectHL Int
    | NewARegisterIndirect IXIY Int
    | SetARegisterIndirect IXIY Int
    | NewMainRegisterIndirect ChangeMainRegister IXIY Int
    | IndexedIndirectIncrement IXIY Int
    | IndexedIndirectDecrement IXIY Int
    | FlagOpIndexedIndirect FlagFunc IXIY Int
    | NewHValue Int
    | NewLValue Int
    | NewIXHValue Int
    | NewIXLValue Int
    | NewIYHValue Int
    | NewIYLValue Int


doubleWithRegisters : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, InstructionDuration )
doubleWithRegisters =
    Dict.fromList
        [ ( 0x26, ( ld_h_n, SevenTStates ) )
        , ( 0x2E, ( ld_l_n, SevenTStates ) )
        , ( 0x36, ( ld_indirect_hl_n, TenTStates ) )
        ]


doubleWithRegistersIX : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, InstructionDuration )
doubleWithRegistersIX =
    Dict.fromList
        [ ( 0x26, ( ld_ix_h_n, ElevenTStates ) )
        , ( 0x2E, ( ld_ix_l_n, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_ix, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_ix, TwentyThreeTStates ) )
        , ( 0x46, ( ld_b_indirect_ix, SevenTStates ) )
        , ( 0x4E, ( ld_c_indirect_ix, SevenTStates ) )
        , ( 0x56, ( ld_d_indirect_ix, SevenTStates ) )
        , ( 0x5E, ( ld_e_indirect_ix, SevenTStates ) )

        -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
        --, ( 0x66, ( \z80_main param -> NewHRegisterIndirect (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x66, ( \z80_main param -> NewMainRegisterIndirect ChangeMainH IXIY_IX (byte param), NineteenTStates ) )

        -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
        --, ( 0x6E, ( \z80_main param -> NewLRegisterIndirect (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x6E, ( \z80_main param -> NewMainRegisterIndirect ChangeMainL IXIY_IX (byte param), NineteenTStates ) )
        , ( 0x86, ( \z80_main param -> FlagOpIndexedIndirect AddA IXIY_IX (byte param), NineteenTStates ) )
        , ( 0x8E, ( \z80_main param -> FlagOpIndexedIndirect AdcA IXIY_IX (byte param), NineteenTStates ) )
        , ( 0x96, ( \z80_main param -> FlagOpIndexedIndirect SubA IXIY_IX (byte param), NineteenTStates ) )
        , ( 0x9E, ( \z80_main param -> FlagOpIndexedIndirect SbcA IXIY_IX (byte param), NineteenTStates ) )
        , ( 0xA6, ( \z80_main param -> FlagOpIndexedIndirect AndA IXIY_IX (byte param), NineteenTStates ) )
        , ( 0xAE, ( \z80_main param -> FlagOpIndexedIndirect XorA IXIY_IX (byte param), NineteenTStates ) )
        , ( 0xB6, ( \z80_main param -> FlagOpIndexedIndirect OrA IXIY_IX (byte param), NineteenTStates ) )
        , ( 0xBE, ( \z80_main param -> FlagOpIndexedIndirect CpA IXIY_IX (byte param), NineteenTStates ) )
        , ( 0x77, ( ld_indirect_ix_a, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_ix, NineteenTStates ) )
        ]


doubleWithRegistersIY : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, InstructionDuration )
doubleWithRegistersIY =
    Dict.fromList
        [ ( 0x26, ( ld_iy_h_n, ElevenTStates ) )
        , ( 0x2E, ( ld_iy_l_n, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_iy, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_iy, TwentyThreeTStates ) )
        , ( 0x46, ( ld_b_indirect_iy, SevenTStates ) )
        , ( 0x4E, ( ld_c_indirect_iy, SevenTStates ) )
        , ( 0x56, ( ld_d_indirect_iy, SevenTStates ) )
        , ( 0x5E, ( ld_e_indirect_iy, SevenTStates ) )

        -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
        --, ( 0x66, ( \z80_main param -> NewHRegisterIndirect (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x66, ( \z80_main param -> NewMainRegisterIndirect ChangeMainH IXIY_IY (byte param), NineteenTStates ) )

        -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
        --, ( 0x6E, ( \z80_main param -> NewLRegisterIndirect (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x6E, ( \z80_main param -> NewMainRegisterIndirect ChangeMainL IXIY_IY (byte param), NineteenTStates ) )
        , ( 0x86, ( \z80_main param -> FlagOpIndexedIndirect AddA IXIY_IY (byte param), NineteenTStates ) )
        , ( 0x8E, ( \z80_main param -> FlagOpIndexedIndirect AdcA IXIY_IY (byte param), NineteenTStates ) )
        , ( 0x96, ( \z80_main param -> FlagOpIndexedIndirect SubA IXIY_IY (byte param), NineteenTStates ) )
        , ( 0x9E, ( \z80_main param -> FlagOpIndexedIndirect SbcA IXIY_IY (byte param), NineteenTStates ) )
        , ( 0xA6, ( \z80_main param -> FlagOpIndexedIndirect AndA IXIY_IY (byte param), NineteenTStates ) )
        , ( 0xAE, ( \z80_main param -> FlagOpIndexedIndirect XorA IXIY_IY (byte param), NineteenTStates ) )
        , ( 0xB6, ( \z80_main param -> FlagOpIndexedIndirect OrA IXIY_IY (byte param), NineteenTStates ) )
        , ( 0xBE, ( \z80_main param -> FlagOpIndexedIndirect CpA IXIY_IY (byte param), NineteenTStates ) )
        , ( 0x77, ( ld_indirect_iy_a, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_iy, NineteenTStates ) )
        ]


ld_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_h_n _ param =
    -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
    --Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF) |> NewHLRegisterValue
    NewHValue param


ld_ix_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_h_n _ param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    --Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.ix 0xFF) |> NewIXRegisterValue
    NewIXHValue param


ld_iy_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_iy_h_n _ param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    --Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.iy 0xFF) |> NewIYRegisterValue
    NewIYHValue param


ld_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_l_n _ param =
    -- case 0x2E: HL=HL&0xFF00|imm8(); break;
    --Bitwise.or param (Bitwise.and z80_main.hl 0xFF00) |> NewHLRegisterValue
    NewLValue param


ld_ix_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_l_n _ param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    --Bitwise.or param (Bitwise.and z80_main.ix 0xFF00) |> NewIXRegisterValue
    NewIXLValue param


ld_iy_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_iy_l_n _ param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    --Bitwise.or param (Bitwise.and z80_main.iy 0xFF00) |> NewIYRegisterValue
    NewIYLValue param


ld_b_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_b_indirect_ix z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    --NewBRegisterIndirect address
    NewMainRegisterIndirect ChangeMainB IXIY_IX (byte param)


ld_indirect_ix_a : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_indirect_ix_a z80_main param =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    SetARegisterIndirect IXIY_IX (byte param)


ld_indirect_iy_a : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_indirect_iy_a z80_main param =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    SetARegisterIndirect IXIY_IY (byte param)


ld_a_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_a_indirect_ix z80_main param =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    NewARegisterIndirect IXIY_IX (byte param)


ld_a_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_a_indirect_iy z80_main param =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    NewARegisterIndirect IXIY_IY (byte param)


ld_b_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_b_indirect_iy z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    --NewBRegisterIndirect address
    NewMainRegisterIndirect ChangeMainB IXIY_IY (byte param)


ld_c_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_c_indirect_ix z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    --NewCRegisterIndirect address
    NewMainRegisterIndirect ChangeMainC IXIY_IX (byte param)


ld_c_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_c_indirect_iy z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    --NewCRegisterIndirect address
    NewMainRegisterIndirect ChangeMainC IXIY_IY (byte param)


ld_indirect_hl_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_indirect_hl_n z80_main param =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    DoubleRegChangeStoreIndirectHL param


inc_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
inc_indirect_ix z80_main param =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement IXIY_IX (param |> byte)


inc_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
inc_indirect_iy z80_main param =
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement IXIY_IY (param |> byte)


dec_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
dec_indirect_ix z80_main param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement IXIY_IX param


dec_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
dec_indirect_iy z80_main param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement IXIY_IY param


ld_d_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_d_indirect_ix z80_main param =
    --case 0x56: D=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    --NewDRegisterIndirect address
    NewMainRegisterIndirect ChangeMainD IXIY_IX (byte param)



--    NewDIndexedIndirect z80_main.ix param


ld_e_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_e_indirect_ix z80_main param =
    --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    --NewERegisterIndirect address
    NewMainRegisterIndirect ChangeMainE IXIY_IX (byte param)



--NewEIndexedIndirect z80_main.ix param


ld_d_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_d_indirect_iy z80_main param =
    --case 0x56: D=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    --NewDRegisterIndirect address
    NewMainRegisterIndirect ChangeMainD IXIY_IY (byte param)


ld_e_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_e_indirect_iy z80_main param =
    --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    --NewERegisterIndirect address
    NewMainRegisterIndirect ChangeMainE IXIY_IY (byte param)


applyDoubleWithRegistersDelta : MediumPCIncrement -> CpuTimeCTime -> DoubleWithRegisterChange -> Z80ROM -> Z80Core -> Z80Core
applyDoubleWithRegistersDelta pc_inc cpu_time z80changeData rom48k z80 =
    let
        new_pc =
            case pc_inc of
                IncreaseByTwo ->
                    z80.pc + 2

                IncreaseByThree ->
                    z80.pc + 3
    in
    case z80changeData of
        --RelativeJumpWithTimeOffset single8BitChange regvalue maybeInt timeOffset ->
        --    let
        --        pc =
        --            case maybeInt of
        --                Just jump ->
        --                    Bitwise.and (new_pc + jump) 0xFFFF
        --
        --                Nothing ->
        --                    Bitwise.and new_pc 0xFFFF
        --
        --        main =
        --            z80.main |> applySimple8BitChange single8BitChange regvalue
        --    in
        --    { z80
        --        | main = main
        --        , pc = pc
        --        , clockTime = cpu_time |> addCpuTimeTime timeOffset
        --    }
        DoubleRegChangeStoreIndirectHL value ->
            let
                addr =
                    z80.main.hl

                pc =
                    Bitwise.and new_pc 0xFFFF

                ( env_1, newTime ) =
                    z80.env |> setMem addr value cpu_time
            in
            { z80
                | pc = pc
                , clockTime = newTime
                , env = env_1
            }

        NewHValue param ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
            in
            { z80
                | pc = pc
                , clockTime = cpu_time
                , main = { z80_main | hl = new_hl }
            }

        NewLValue param ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or param (Bitwise.and z80_main.hl 0xFF00)
            in
            { z80
                | pc = pc
                , clockTime = cpu_time
                , main = { z80_main | hl = new_hl }
            }

        NewIXHValue param ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.ix 0xFF)
            in
            { z80
                | pc = pc
                , clockTime = cpu_time
                , main = { z80_main | ix = new_hl }
            }

        NewIXLValue param ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or param (Bitwise.and z80_main.ix 0xFF00)
            in
            { z80
                | pc = pc
                , clockTime = cpu_time
                , main = { z80_main | ix = new_hl }
            }

        NewIYHValue param ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.iy 0xFF)
            in
            { z80
                | pc = pc
                , clockTime = cpu_time
                , main = { z80_main | iy = new_hl }
            }

        NewIYLValue param ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                z80_main =
                    z80.main

                new_hl =
                    Bitwise.or param (Bitwise.and z80_main.iy 0xFF00)
            in
            { z80
                | pc = pc
                , clockTime = cpu_time
                , main = { z80_main | iy = new_hl }
            }

        --NewBRegisterIndirect addr ->
        --    let
        --        pc =
        --            Bitwise.and new_pc 0xFFFF
        --
        --        main =
        --            z80.main
        --
        --        new_b =
        --            z80.env |> mem addr cpu_time rom48k
        --    in
        --    { z80
        --        | pc = pc
        --        , clockTime = new_b.time
        --        , main = { main | b = new_b.value }
        --    }
        NewMainRegisterIndirect changeMainRegister ixiy param ->
            let
                addr =
                    case ixiy of
                        IXIY_IX ->
                            z80.main.ix + param

                        IXIY_IY ->
                            z80.main.iy + param

                pc =
                    Bitwise.and new_pc 0xFFFF

                main =
                    z80.main

                new_b =
                    z80.env |> mem addr cpu_time rom48k

                new_main =
                    case changeMainRegister of
                        ChangeMainB ->
                            { main | b = new_b.value }

                        ChangeMainC ->
                            { main | c = new_b.value }

                        ChangeMainD ->
                            { main | d = new_b.value }

                        ChangeMainE ->
                            { main | e = new_b.value }

                        ChangeMainH ->
                            { main | hl = Bitwise.or (main.hl |> Bitwise.and 0xFF) (new_b.value |> shiftLeftBy8) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or (main.hl |> Bitwise.and 0xFF00) (new_b.value |> Bitwise.and 0xFF) }
            in
            { z80
                | pc = pc
                , clockTime = new_b.time
                , main = new_main
            }

        NewARegisterIndirect ixiy param ->
            let
                addr =
                    case ixiy of
                        IXIY_IX ->
                            z80.main.ix + param

                        IXIY_IY ->
                            z80.main.iy + param

                pc =
                    Bitwise.and new_pc 0xFFFF

                flags =
                    z80.flags

                new_a =
                    z80.env |> mem addr cpu_time rom48k
            in
            { z80
                | pc = pc
                , clockTime = new_a.time
                , flags = { flags | a = new_a.value }
            }

        SetARegisterIndirect ixiy param ->
            let
                addr =
                    case ixiy of
                        IXIY_IX ->
                            z80.main.ix + param

                        IXIY_IY ->
                            z80.main.iy + param

                pc =
                    Bitwise.and new_pc 0xFFFF

                ( env_1, newTime ) =
                    z80.env |> setMem addr z80.flags.a cpu_time
            in
            { z80
                | pc = pc
                , clockTime = newTime
                , env = env_1
            }

        --NewCRegisterIndirect addr ->
        --    let
        --        pc =
        --            Bitwise.and new_pc 0xFFFF
        --
        --        main =
        --            z80.main
        --
        --        new_b =
        --            z80.env |> mem addr cpu_time rom48k
        --    in
        --    { z80
        --        | pc = pc
        --        , clockTime = new_b.time
        --        , main = { main | c = new_b.value }
        --    }
        --NewDRegisterIndirect addr ->
        --    let
        --        pc =
        --            Bitwise.and new_pc 0xFFFF
        --
        --        main =
        --            z80.main
        --
        --        new_b =
        --            z80.env |> mem addr cpu_time rom48k
        --    in
        --    { z80
        --        | pc = pc
        --        , clockTime = new_b.time
        --        , main = { main | d = new_b.value }
        --    }
        --NewERegisterIndirect addr ->
        --    let
        --        pc =
        --            Bitwise.and new_pc 0xFFFF
        --
        --        --env_1 =
        --        --    { old_env | time = cpu_time }
        --        main =
        --            z80.main
        --
        --        new_b =
        --            z80.env |> mem addr cpu_time rom48k
        --    in
        --    { z80
        --        | pc = pc
        --        , clockTime = new_b.time
        --        , main = { main | e = new_b.value }
        --    }
        --NewHRegisterIndirect addr ->
        --    let
        --        pc =
        --            Bitwise.and new_pc 0xFFFF
        --
        --        main =
        --            z80.main
        --
        --        new_b =
        --            z80.env |> mem addr cpu_time rom48k
        --    in
        --    { z80
        --        | pc = pc
        --        , clockTime = new_b.time
        --        , main = { main | hl = Bitwise.or (main.hl |> Bitwise.and 0xFF) (new_b.value |> shiftLeftBy8) }
        --    }
        --
        --NewLRegisterIndirect addr ->
        --    let
        --        pc =
        --            Bitwise.and new_pc 0xFFFF
        --
        --        main =
        --            z80.main
        --
        --        new_b =
        --            z80.env |> mem addr cpu_time rom48k
        --    in
        --    { z80
        --        | pc = pc
        --        , clockTime = new_b.time
        --        , main = { main | hl = Bitwise.or (main.hl |> Bitwise.and 0xFF00) (new_b.value |> Bitwise.and 0xFF) }
        --    }
        FlagOpIndexedIndirect flagFunc ixiy offset ->
            let
                addr =
                    case ixiy of
                        IXIY_IX ->
                            z80.main.ix

                        IXIY_IY ->
                            z80.main.iy

                flags =
                    z80.flags

                address =
                    addr + offset |> Bitwise.and 0xFFFF

                pc =
                    Bitwise.and new_pc 0xFFFF

                value =
                    z80.env |> mem address cpu_time rom48k
            in
            { z80
                | pc = pc
                , flags = flags |> changeFlags flagFunc value.value
                , clockTime = value.time
            }

        IndexedIndirectIncrement ixiy offset ->
            let
                inAddr =
                    case ixiy of
                        IXIY_IX ->
                            z80.main.ix

                        IXIY_IY ->
                            z80.main.iy

                base_addr =
                    inAddr + offset |> Bitwise.and 0xFFFF

                ramAddr =
                    base_addr - 0x4000

                pc =
                    Bitwise.and new_pc 0xFFFF
            in
            if ramAddr >= 0 then
                let
                    value =
                        z80.env |> mem base_addr cpu_time rom48k

                    valueWithFlags =
                        z80.flags |> inc value.value

                    ( env_1, newTime ) =
                        z80.env |> setMem base_addr valueWithFlags.value value.time
                in
                { z80
                    | pc = pc
                    , env = env_1
                    , clockTime = newTime
                    , flags = valueWithFlags.flags
                }

            else
                { z80
                    | pc = pc
                    , clockTime = cpu_time
                }

        IndexedIndirectDecrement ixiy offset ->
            let
                inAddr =
                    case ixiy of
                        IXIY_IX ->
                            z80.main.ix

                        IXIY_IY ->
                            z80.main.iy

                base_addr =
                    inAddr + byte offset |> Bitwise.and 0xFFFF

                ramAddr =
                    base_addr - 0x4000

                pc =
                    Bitwise.and new_pc 0xFFFF
            in
            if ramAddr >= 0 then
                let
                    value =
                        z80.env |> mem base_addr cpu_time rom48k

                    valueWithFlags =
                        z80.flags |> dec value.value

                    ( env_1, newTime ) =
                        z80.env |> setMem base_addr valueWithFlags.value value.time
                in
                { z80
                    | pc = pc
                    , clockTime = newTime
                    , env = env_1
                    , flags = valueWithFlags.flags
                }

            else
                { z80
                    | pc = pc
                    , clockTime = cpu_time
                }
