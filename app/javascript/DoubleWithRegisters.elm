module DoubleWithRegisters exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..), addCpuTimeTime)
import Dict exposing (Dict)
import PCIncrement exposing (MediumPCIncrement(..))
import SingleWith8BitParameter exposing (Single8BitChange(..), applySimple8BitChange)
import Utils exposing (byte, shiftLeftBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (getRamValue, mem, setMem, setRam)
import Z80Flags exposing (FlagFunc(..), changeFlags, dec, inc)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


type DoubleWithRegisterChange
    = RelativeJumpWithTimeOffset Single8BitChange (Maybe Int) Int
    | DoubleRegChangeStoreIndirect Int Int
    | NewHLRegisterValue Int
    | NewIXRegisterValue Int
    | NewIYRegisterValue Int
    | NewARegisterIndirect Int
    | SetARegisterIndirect Int
    | NewBRegisterIndirect Int
    | NewCRegisterIndirect Int
    | NewDRegisterIndirect Int
    | NewERegisterIndirect Int
    | NewHRegisterIndirect Int
    | NewLRegisterIndirect Int
    | IndexedIndirectIncrement Int Int
    | IndexedIndirectDecrement Int Int
    | FlagOpIndexedIndirect FlagFunc Int Int


doubleWithRegisters : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement, InstructionDuration )
doubleWithRegisters =
    Dict.fromList
        [ --  another 5 if jump actually taken
          ( 0x10, ( djnz, IncreaseByTwo, EightTStates ) )
        , ( 0x26, ( ld_h_n, IncreaseByTwo, SevenTStates ) )
        , ( 0x2E, ( ld_l_n, IncreaseByTwo, SevenTStates ) )
        , ( 0x36, ( ld_indirect_hl_n, IncreaseByTwo, TenTStates ) )
        ]


doubleWithRegistersIX : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement, InstructionDuration )
doubleWithRegistersIX =
    Dict.fromList
        [ ( 0x26, ( ld_ix_h_n, IncreaseByThree, ElevenTStates ) )
        , ( 0x2E, ( ld_ix_l_n, IncreaseByThree, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_ix, IncreaseByThree, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_ix, IncreaseByThree, TwentyThreeTStates ) )
        , ( 0x46, ( ld_b_indirect_ix, IncreaseByThree, SevenTStates ) )
        , ( 0x4E, ( ld_c_indirect_ix, IncreaseByThree, SevenTStates ) )
        , ( 0x56, ( ld_d_indirect_ix, IncreaseByThree, SevenTStates ) )
        , ( 0x5E, ( ld_e_indirect_ix, IncreaseByThree, SevenTStates ) )
        , ( 0x66, ( \z80_main param -> NewHRegisterIndirect (z80_main.ix + byte param), IncreaseByThree, NineteenTStates ) )
        , ( 0x6E, ( \z80_main param -> NewLRegisterIndirect (z80_main.ix + byte param), IncreaseByThree, NineteenTStates ) )
        , ( 0x86, ( \z80_main param -> FlagOpIndexedIndirect AddA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0x8E, ( \z80_main param -> FlagOpIndexedIndirect AdcA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0x96, ( \z80_main param -> FlagOpIndexedIndirect SubA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0x9E, ( \z80_main param -> FlagOpIndexedIndirect SbcA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0xA6, ( \z80_main param -> FlagOpIndexedIndirect AndA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0xAE, ( \z80_main param -> FlagOpIndexedIndirect XorA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0xB6, ( \z80_main param -> FlagOpIndexedIndirect OrA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0xBE, ( \z80_main param -> FlagOpIndexedIndirect CpA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0x77, ( ld_indirect_ix_a, IncreaseByThree, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_ix, IncreaseByThree, NineteenTStates ) )
        ]


doubleWithRegistersIY : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement, InstructionDuration )
doubleWithRegistersIY =
    Dict.fromList
        [ ( 0x26, ( ld_iy_h_n, IncreaseByThree, ElevenTStates ) )
        , ( 0x2E, ( ld_iy_l_n, IncreaseByThree, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_iy, IncreaseByThree, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_iy, IncreaseByThree, TwentyThreeTStates ) )
        , ( 0x46, ( ld_b_indirect_iy, IncreaseByThree, SevenTStates ) )
        , ( 0x4E, ( ld_c_indirect_iy, IncreaseByThree, SevenTStates ) )
        , ( 0x56, ( ld_d_indirect_iy, IncreaseByThree, SevenTStates ) )
        , ( 0x5E, ( ld_e_indirect_iy, IncreaseByThree, SevenTStates ) )
        , ( 0x66, ( \z80_main param -> NewHRegisterIndirect (z80_main.iy + byte param), IncreaseByThree, NineteenTStates ) )
        , ( 0x6E, ( \z80_main param -> NewLRegisterIndirect (z80_main.iy + byte param), IncreaseByThree, NineteenTStates ) )
        , ( 0x86, ( \z80_main param -> FlagOpIndexedIndirect AddA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0x8E, ( \z80_main param -> FlagOpIndexedIndirect AdcA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0x96, ( \z80_main param -> FlagOpIndexedIndirect SubA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0x9E, ( \z80_main param -> FlagOpIndexedIndirect SbcA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0xA6, ( \z80_main param -> FlagOpIndexedIndirect AndA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0xAE, ( \z80_main param -> FlagOpIndexedIndirect XorA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0xB6, ( \z80_main param -> FlagOpIndexedIndirect OrA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0xBE, ( \z80_main param -> FlagOpIndexedIndirect CpA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0x77, ( ld_indirect_iy_a, IncreaseByThree, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_iy, IncreaseByThree, NineteenTStates ) )
        ]


ld_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_h_n z80_main param =
    -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF) |> NewHLRegisterValue


ld_ix_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_h_n z80_main param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.ix 0xFF) |> NewIXRegisterValue


ld_iy_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_iy_h_n z80_main param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.iy 0xFF) |> NewIYRegisterValue


ld_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_l_n z80_main param =
    -- case 0x2E: HL=HL&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.hl 0xFF00) |> NewHLRegisterValue


ld_ix_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_l_n z80_main param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.ix 0xFF00) |> NewIXRegisterValue


ld_iy_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_iy_l_n z80_main param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.iy 0xFF00) |> NewIYRegisterValue


ld_b_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_b_indirect_ix z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    NewBRegisterIndirect address


ld_indirect_ix_a : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_indirect_ix_a z80_main param =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    SetARegisterIndirect address


ld_indirect_iy_a : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_indirect_iy_a z80_main param =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    SetARegisterIndirect address


ld_a_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_a_indirect_ix z80_main param =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    NewARegisterIndirect address


ld_a_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_a_indirect_iy z80_main param =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    NewARegisterIndirect address


ld_b_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_b_indirect_iy z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    NewBRegisterIndirect address


ld_c_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_c_indirect_ix z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    NewCRegisterIndirect address


ld_c_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_c_indirect_iy z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    NewCRegisterIndirect address


djnz : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
djnz z80_main param =
    --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
    --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
    --PC=(char)v;} break;
    let
        d =
            byte param

        b =
            Bitwise.and (z80_main.b - 1) 0xFF

        ( time, jump ) =
            if b /= 0 then
                ( 9, Just d )

            else
                ( 4, Nothing )
    in
    RelativeJumpWithTimeOffset (NewBRegister b) jump time


ld_indirect_hl_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_indirect_hl_n z80_main param =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    DoubleRegChangeStoreIndirect z80_main.hl param


inc_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
inc_indirect_ix z80_main param =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement z80_main.ix param


inc_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
inc_indirect_iy z80_main param =
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement z80_main.iy param


dec_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
dec_indirect_ix z80_main param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement z80_main.ix param


dec_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
dec_indirect_iy z80_main param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement z80_main.iy param


ld_d_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_d_indirect_ix z80_main param =
    --case 0x56: D=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    NewDRegisterIndirect address



--    NewDIndexedIndirect z80_main.ix param


ld_e_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_e_indirect_ix z80_main param =
    --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    NewERegisterIndirect address



--NewEIndexedIndirect z80_main.ix param


ld_d_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_d_indirect_iy z80_main param =
    --case 0x56: D=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    NewDRegisterIndirect address


ld_e_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_e_indirect_iy z80_main param =
    --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    NewERegisterIndirect address


applyDoubleWithRegistersDelta : MediumPCIncrement -> CpuTimeCTime -> DoubleWithRegisterChange -> Z80ROM -> Z80Core -> Z80Core
applyDoubleWithRegistersDelta pc_inc cpu_time z80changeData rom48k z80 =
    let
        old_env =
            z80.env

        new_pc =
            case pc_inc of
                IncreaseByTwo ->
                    z80.pc + 2

                IncreaseByThree ->
                    z80.pc + 3
    in
    case z80changeData of
        RelativeJumpWithTimeOffset single8BitChange maybeInt timeOffset ->
            let
                pc =
                    case maybeInt of
                        Just jump ->
                            Bitwise.and (new_pc + jump) 0xFFFF

                        Nothing ->
                            Bitwise.and new_pc 0xFFFF

                main =
                    z80.main |> applySimple8BitChange single8BitChange
            in
            { z80
                | main = main
                , pc = pc
                , env = { old_env | time = cpu_time |> addCpuTimeTime timeOffset }
            }

        DoubleRegChangeStoreIndirect addr value ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time } |> setMem addr value
            in
            { z80
                | pc = pc
                , env = env_1
            }

        NewHLRegisterValue int ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main
            in
            { z80
                | pc = pc
                , env = env_1
                , main = { main | hl = int }
            }

        NewIXRegisterValue int ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main
            in
            { z80
                | pc = pc
                , env = env_1
                , main = { main | ix = int }
            }

        NewIYRegisterValue int ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main
            in
            { z80
                | pc = pc
                , env = env_1
                , main = { main | iy = int }
            }

        NewBRegisterIndirect addr ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main

                new_b =
                    env_1 |> mem addr env_1.time rom48k
            in
            { z80
                | pc = pc
                , env = { env_1 | time = new_b.time }
                , main = { main | b = new_b.value }
            }

        NewARegisterIndirect addr ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                flags =
                    z80.flags

                new_a =
                    env_1 |> mem addr env_1.time rom48k
            in
            { z80
                | pc = pc
                , env = { env_1 | time = new_a.time }
                , flags = { flags | a = new_a.value }
            }

        SetARegisterIndirect addr ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time } |> setMem addr z80.flags.a
            in
            { z80
                | pc = pc
                , env = env_1
            }

        NewCRegisterIndirect addr ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main

                new_b =
                    env_1 |> mem addr env_1.time rom48k
            in
            { z80
                | pc = pc
                , env = { env_1 | time = new_b.time }
                , main = { main | c = new_b.value }
            }

        NewDRegisterIndirect addr ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main

                new_b =
                    env_1 |> mem addr env_1.time rom48k
            in
            { z80
                | pc = pc
                , env = { env_1 | time = new_b.time }
                , main = { main | d = new_b.value }
            }

        NewERegisterIndirect addr ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main

                new_b =
                    env_1 |> mem addr env_1.time rom48k
            in
            { z80
                | pc = pc
                , env = { env_1 | time = new_b.time }
                , main = { main | e = new_b.value }
            }

        NewHRegisterIndirect addr ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main

                new_b =
                    env_1 |> mem addr env_1.time rom48k
            in
            { z80
                | pc = pc
                , env = { env_1 | time = new_b.time }
                , main = { main | hl = Bitwise.or (main.hl |> Bitwise.and 0xFF) (new_b.value |> shiftLeftBy8) }
            }

        NewLRegisterIndirect addr ->
            let
                pc =
                    Bitwise.and new_pc 0xFFFF

                env_1 =
                    { old_env | time = cpu_time }

                main =
                    z80.main

                new_b =
                    env_1 |> mem addr env_1.time rom48k
            in
            { z80
                | pc = pc
                , env = { env_1 | time = new_b.time }
                , main = { main | hl = Bitwise.or (main.hl |> Bitwise.and 0xFF00) (new_b.value |> Bitwise.and 0xFF) }
            }

        FlagOpIndexedIndirect flagFunc addr offset ->
            let
                flags =
                    z80.flags

                env_1 =
                    { old_env | time = cpu_time }

                address =
                    addr + byte offset |> Bitwise.and 0xFFFF

                pc =
                    Bitwise.and new_pc 0xFFFF

                value =
                    env_1 |> mem address env_1.time rom48k
            in
            { z80
                | pc = pc
                , flags = flags |> changeFlags flagFunc value.value
                , env = { env_1 | time = value.time }
            }

        --Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeFlagRegisters (z80.flags |> adc value.value) }
        IndexedIndirectIncrement addr offset ->
            let
                ramAddr =
                    (addr + byte offset |> Bitwise.and 0xFFFF) - 0x4000

                pc =
                    Bitwise.and new_pc 0xFFFF
            in
            if ramAddr >= 0 then
                let
                    value =
                        old_env |> getRamValue ramAddr rom48k

                    valueWithFlags =
                        z80.flags |> inc value

                    env_1 =
                        --{ old_env | ram = old_env.ram |> setRamValue ramAddr valueWithFlags.value }
                        old_env |> setRam ramAddr valueWithFlags.value
                in
                { z80
                    | pc = pc
                    , env = env_1
                    , flags = valueWithFlags.flags
                }

            else
                { z80
                    | pc = pc
                }

        IndexedIndirectDecrement addr offset ->
            let
                ramAddr =
                    (addr + byte offset |> Bitwise.and 0xFFFF) - 0x4000

                pc =
                    Bitwise.and new_pc 0xFFFF
            in
            if ramAddr >= 0 then
                let
                    value =
                        old_env |> getRamValue ramAddr rom48k

                    valueWithFlags =
                        z80.flags |> dec value

                    env_1 =
                        --{ old_env | ram = old_env.ram |> setRamValue ramAddr valueWithFlags.value }
                        old_env |> setRam ramAddr valueWithFlags.value
                in
                { z80
                    | pc = pc
                    , env = env_1
                    , flags = valueWithFlags.flags
                }

            else
                { z80
                    | pc = pc
                }
