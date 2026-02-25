module DoubleWithRegisters exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import SingleWith8BitParameter exposing (JumpChange(..))
import Utils exposing (byte, shiftLeftBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (setMem)
import Z80Flags exposing (FlagFunc(..), changeFlags, dec, inc)
import Z80Mem exposing (mem)
import Z80Registers exposing (ChangeMainRegister(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


type DoubleWithRegisterChange
    = NewIXHRegisterValue Int
    | NewIXLRegisterValue Int
    | NewIYHRegisterValue Int
    | NewIYLRegisterValue Int
    | NewARegisterIndirect (MainWithIndexRegisters -> Int) Int
    | SetARegisterIndirect (MainWithIndexRegisters -> Int) Int
    | IndexedIndirectIncrement Int Int
    | IndexedIndirectDecrement Int Int
    | FlagOpIndexedIndirect FlagFunc Int
    | NewRegisterIndirect ChangeMainRegister Int
    | RegStore8BitValue Int (MainWithIndexRegisters -> Int) Int


doubleWithRegisters : Dict Int ( Int -> JumpChange, InstructionDuration )
doubleWithRegisters =
    Dict.fromList
        [ ( 0x26, ( ld_h_n, SevenTStates ) )
        , ( 0x2E, ( ld_l_n, SevenTStates ) )
        , ( 0x36, ( ld_indirect_hl_n, TenTStates ) )
        ]


doubleWithRegistersIX : Dict Int ( Int -> MainWithIndexRegisters -> DoubleWithRegisterChange, InstructionDuration )
doubleWithRegistersIX =
    Dict.fromList
        [ ( 0x26, ( ld_ix_h_n, ElevenTStates ) )
        , ( 0x2E, ( ld_ix_l_n, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_ix, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_ix, TwentyThreeTStates ) )

        -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
        , ( 0x46, ( \param z80_main -> NewRegisterIndirect ChangeMainB (z80_main.ix + byte param |> Bitwise.and 0xFFFF), SevenTStates ) )

        -- case 0x4E: C=env.mem(getd(xy)); time+=3; break;
        , ( 0x4E, ( \param z80_main -> NewRegisterIndirect ChangeMainC (z80_main.ix + byte param |> Bitwise.and 0xFFFF), SevenTStates ) )

        --case 0x56: D=env.mem(getd(xy)); time+=3; break;
        , ( 0x56, ( \param z80_main -> NewRegisterIndirect ChangeMainD (z80_main.ix + byte param |> Bitwise.and 0xFFFF), SevenTStates ) )

        --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
        , ( 0x5E, ( \param z80_main -> NewRegisterIndirect ChangeMainE (z80_main.ix + byte param |> Bitwise.and 0xFFFF), SevenTStates ) )

        -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
        , ( 0x66, ( \param z80_main -> NewRegisterIndirect ChangeMainH (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )

        -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
        , ( 0x6E, ( \param z80_main -> NewRegisterIndirect ChangeMainL (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x86, ( \param z80_main -> FlagOpIndexedIndirect AddA (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x8E, ( \param z80_main -> FlagOpIndexedIndirect AdcA (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x96, ( \param z80_main -> FlagOpIndexedIndirect SubA (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x9E, ( \param z80_main -> FlagOpIndexedIndirect SbcA (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0xA6, ( \param z80_main -> FlagOpIndexedIndirect AndA (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0xAE, ( \param z80_main -> FlagOpIndexedIndirect XorA (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0xB6, ( \param z80_main -> FlagOpIndexedIndirect OrA (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0xBE, ( \param z80_main -> FlagOpIndexedIndirect CpA (z80_main.ix + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x77, ( ld_indirect_ix_a, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_ix, NineteenTStates ) )
        ]


doubleWithRegistersIY : Dict Int ( Int -> MainWithIndexRegisters -> DoubleWithRegisterChange, InstructionDuration )
doubleWithRegistersIY =
    Dict.fromList
        [ ( 0x26, ( ld_iy_h_n, ElevenTStates ) )
        , ( 0x2E, ( ld_iy_l_n, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_iy, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_iy, TwentyThreeTStates ) )

        -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
        , ( 0x46, ( \param z80_main -> NewRegisterIndirect ChangeMainB (z80_main.iy + byte param |> Bitwise.and 0xFFFF), SevenTStates ) )

        -- case 0x4E: C=env.mem(getd(xy)); time+=3; break;
        , ( 0x4E, ( \param z80_main -> NewRegisterIndirect ChangeMainC (z80_main.iy + byte param |> Bitwise.and 0xFFFF), SevenTStates ) )

        --case 0x56: D=env.mem(getd(xy)); time+=3; break;
        , ( 0x56, ( \param z80_main -> NewRegisterIndirect ChangeMainD (z80_main.iy + byte param |> Bitwise.and 0xFFFF), SevenTStates ) )

        --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
        , ( 0x5E, ( \param z80_main -> NewRegisterIndirect ChangeMainE (z80_main.iy + byte param |> Bitwise.and 0xFFFF), SevenTStates ) )

        -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
        , ( 0x66, ( \param z80_main -> NewRegisterIndirect ChangeMainH (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )

        -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
        , ( 0x6E, ( \param z80_main -> NewRegisterIndirect ChangeMainL (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x86, ( \param z80_main -> FlagOpIndexedIndirect AddA (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x8E, ( \param z80_main -> FlagOpIndexedIndirect AdcA (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x96, ( \param z80_main -> FlagOpIndexedIndirect SubA (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x9E, ( \param z80_main -> FlagOpIndexedIndirect SbcA (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0xA6, ( \param z80_main -> FlagOpIndexedIndirect AndA (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0xAE, ( \param z80_main -> FlagOpIndexedIndirect XorA (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0xB6, ( \param z80_main -> FlagOpIndexedIndirect OrA (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0xBE, ( \param z80_main -> FlagOpIndexedIndirect CpA (z80_main.iy + byte param |> Bitwise.and 0xFFFF), NineteenTStates ) )
        , ( 0x77, ( ld_indirect_iy_a, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_iy, NineteenTStates ) )
        ]


ld_h_n : Int -> JumpChange
ld_h_n param =
    -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
    SimpleNewHValue param


ld_ix_h_n : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
ld_ix_h_n param z80_main =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    NewIXHRegisterValue param


ld_iy_h_n : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
ld_iy_h_n param z80_main =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    NewIYHRegisterValue param


ld_l_n : Int -> JumpChange
ld_l_n param =
    -- case 0x2E: HL=HL&0xFF00|imm8(); break;
    SimpleNewLValue param


ld_ix_l_n : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
ld_ix_l_n param z80_main =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    NewIXLRegisterValue param


ld_iy_l_n : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
ld_iy_l_n param z80_main =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    NewIYLRegisterValue param


ld_indirect_ix_a : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
ld_indirect_ix_a param z80_main =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    --SetARegisterIndirect (z80_main.ix + byte param)
    SetARegisterIndirect .ix param


ld_indirect_iy_a : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
ld_indirect_iy_a param z80_main =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    --SetARegisterIndirect (z80_main.iy + byte param)
    SetARegisterIndirect .iy param


ld_a_indirect_ix : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
ld_a_indirect_ix param z80_main =
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    NewARegisterIndirect .ix param


ld_a_indirect_iy : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
ld_a_indirect_iy param z80_main =
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


inc_indirect_ix : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
inc_indirect_ix param z80_main =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement z80_main.ix param


inc_indirect_iy : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
inc_indirect_iy param z80_main =
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement z80_main.iy param


dec_indirect_ix : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
dec_indirect_ix param z80_main =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement z80_main.ix param


dec_indirect_iy : Int -> MainWithIndexRegisters -> DoubleWithRegisterChange
dec_indirect_iy param z80_main =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement z80_main.iy param


applyDoubleWithRegistersDelta : CpuTimeCTime -> DoubleWithRegisterChange -> Z80ROM -> Z80Core -> Z80Core
applyDoubleWithRegistersDelta cpu_time z80changeData rom48k z80 =
    case z80changeData of
        NewIXHRegisterValue param ->
            let
                z80_main =
                    z80.main

                int =
                    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.ix 0xFF)
            in
            { z80 | main = { z80_main | ix = int } }

        NewIXLRegisterValue param ->
            let
                main =
                    z80.main

                int =
                    Bitwise.or param (Bitwise.and main.ix 0xFF00)
            in
            { z80 | main = { main | ix = int } }

        NewIYHRegisterValue param ->
            let
                z80_main =
                    z80.main

                int =
                    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.iy 0xFF)
            in
            { z80 | main = { z80_main | iy = int } }

        NewIYLRegisterValue param ->
            let
                main =
                    z80.main

                int =
                    Bitwise.or param (Bitwise.and main.iy 0xFF00)
            in
            { z80 | main = { main | iy = int } }

        RegStore8BitValue offset address_f value ->
            let
                address =
                    ((z80.main |> address_f) + (offset |> byte)) |> Bitwise.and 0xFFFF

                ( env1, clockTime ) =
                    z80.env |> setMem address value cpu_time
            in
            { z80 | env = env1 }

        NewRegisterIndirect changeOneRegister addr ->
            let
                main =
                    z80.main

                new_b =
                    z80.env |> mem addr cpu_time rom48k

                new_main =
                    case changeOneRegister of
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
                | main = new_main
            }

        NewARegisterIndirect addr_f offset ->
            let
                flags =
                    z80.flags

                addr =
                    ((z80.main |> addr_f) + byte offset) |> Bitwise.and 0xFFFF

                new_a =
                    z80.env |> mem addr cpu_time rom48k
            in
            { z80
                | flags = { flags | a = new_a.value }
            }

        SetARegisterIndirect addr_f param ->
            let
                addr =
                    (z80.main |> addr_f) + byte param

                ( env_1, newTime ) =
                    z80.env |> setMem addr z80.flags.a cpu_time
            in
            { z80
                | env = env_1
            }

        FlagOpIndexedIndirect flagFunc address ->
            let
                flags =
                    z80.flags

                value =
                    z80.env |> mem address cpu_time rom48k
            in
            { z80
                | flags = flags |> changeFlags flagFunc value.value
            }

        IndexedIndirectIncrement inAddr offset ->
            let
                base_addr =
                    inAddr + byte offset |> Bitwise.and 0xFFFF

                ramAddr =
                    base_addr - 0x4000
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
                    | env = env_1
                    , flags = valueWithFlags.flags
                }

            else
                z80

        IndexedIndirectDecrement inAddr offset ->
            let
                base_addr =
                    inAddr + byte offset |> Bitwise.and 0xFFFF

                ramAddr =
                    base_addr - 0x4000
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
                    | env = env_1
                    , flags = valueWithFlags.flags
                }

            else
                z80
