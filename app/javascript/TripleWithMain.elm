module TripleWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..), TriplePCIncrement(..))
import Utils exposing (byte, shiftRightBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env, setMem, setMem16)
import Z80Types exposing (MainWithIndexRegisters)


type TripleMainChange
    = Store16BitValue Int Int
    | Store8BitValue Int Int


tripleMainRegsIX : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, TriplePCIncrement, InstructionDuration )
tripleMainRegsIX =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_ix, TripleIncrementByFour, TwentyTStates ) )
        , ( 0x36, ( ld_indirect_ix_n, TripleIncrementByFour, TwentyTStates ) )
        , ( 0x70, ( \param16 z80_main -> store_reg_indirect param16 z80_main.ix z80_main.b, TripleIncrementByThree, NineteenTStates ) )
        , ( 0x71, ( \param16 z80_main -> store_reg_indirect param16 z80_main.ix z80_main.c, TripleIncrementByThree, NineteenTStates ) )
        , ( 0x72, ( \param16 z80_main -> store_reg_indirect param16 z80_main.ix z80_main.d, TripleIncrementByThree, NineteenTStates ) )
        , ( 0x73, ( \param16 z80_main -> store_reg_indirect param16 z80_main.ix z80_main.e, TripleIncrementByThree, NineteenTStates ) )
        , ( 0x74, ( \param16 z80_main -> store_reg_indirect param16 z80_main.ix (z80_main.hl |> shiftRightBy8), TripleIncrementByThree, NineteenTStates ) )
        , ( 0x75, ( \param16 z80_main -> store_reg_indirect param16 z80_main.ix (z80_main.hl |> Bitwise.and 0xFF), TripleIncrementByThree, NineteenTStates ) )
        ]


tripleMainRegsIY : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, TriplePCIncrement, InstructionDuration )
tripleMainRegsIY =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_iy, TripleIncrementByFour, TwentyTStates ) )
        , ( 0x36, ( ld_indirect_iy_n, TripleIncrementByFour, TwentyTStates ) )
        , ( 0x70, ( \param16 z80_main -> store_reg_indirect param16 z80_main.iy z80_main.b, TripleIncrementByThree, NineteenTStates ) )
        , ( 0x71, ( \param16 z80_main -> store_reg_indirect param16 z80_main.iy z80_main.c, TripleIncrementByThree, NineteenTStates ) )
        , ( 0x72, ( \param16 z80_main -> store_reg_indirect param16 z80_main.iy z80_main.d, TripleIncrementByThree, NineteenTStates ) )
        , ( 0x73, ( \param16 z80_main -> store_reg_indirect param16 z80_main.iy z80_main.e, TripleIncrementByThree, NineteenTStates ) )
        , ( 0x74, ( \param16 z80_main -> store_reg_indirect param16 z80_main.iy (z80_main.hl |> shiftRightBy8), TripleIncrementByThree, NineteenTStates ) )
        , ( 0x75, ( \param16 z80_main -> store_reg_indirect param16 z80_main.iy (z80_main.hl |> Bitwise.and 0xFF), TripleIncrementByThree, NineteenTStates ) )
        ]


applyTripleMainChange : CpuTimeCTime -> TriplePCIncrement -> TripleMainChange -> Z80Core -> Z80Core
applyTripleMainChange time pcInc z80changeData z80 =
    let
        env =
            z80.env

        new_pc =
            case pcInc of
                TripleIncrementByThree ->
                    Bitwise.and (z80.pc + 3) 0xFFFF

                TripleIncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF
    in
    case z80changeData of
        Store16BitValue address value ->
            let
                ( env1, clockTime ) =
                    env |> setMem16 address value time
            in
            { z80
                | pc = new_pc
                , env = env1
            }

        Store8BitValue address value ->
            let
                ( env1, clockTime ) =
                    env |> setMem address value time
            in
            { z80
                | pc = new_pc
                , env = env1
            }


ld_nn_indirect_ix : Int -> MainWithIndexRegisters -> TripleMainChange
ld_nn_indirect_ix param16 z80_main =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,xy); time+=6; break;
    Store16BitValue param16 z80_main.ix


ld_nn_indirect_iy : Int -> MainWithIndexRegisters -> TripleMainChange
ld_nn_indirect_iy param16 z80_main =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,xy); time+=6; break;
    Store16BitValue param16 z80_main.iy


ld_indirect_ix_n : Int -> MainWithIndexRegisters -> TripleMainChange
ld_indirect_ix_n param16 z80_main =
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    let
        offset =
            param16 |> Bitwise.and 0xFF |> byte

        value =
            param16 |> shiftRightBy8
    in
    Store8BitValue ((z80_main.ix + offset) |> Bitwise.and 0xFFFF) value


ld_indirect_iy_n : Int -> MainWithIndexRegisters -> TripleMainChange
ld_indirect_iy_n param16 z80_main =
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    let
        offset =
            param16 |> Bitwise.and 0xFF |> byte

        value =
            param16 |> shiftRightBy8
    in
    Store8BitValue ((z80_main.iy + offset) |> Bitwise.and 0xFFFF) value


store_reg_indirect : Int -> Int -> Int -> TripleMainChange
store_reg_indirect param16 source dest =
    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
    let
        offset =
            param16 |> Bitwise.and 0xFF |> byte
    in
    Store8BitValue ((source + offset) |> Bitwise.and 0xFFFF) dest
