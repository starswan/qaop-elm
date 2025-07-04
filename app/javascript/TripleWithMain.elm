module TripleWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import PCIncrement exposing (TriplePCIncrement(..))
import Utils exposing (byte, shiftRightBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env, setMem, setMem16)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


type TripleMainChange
    = Store16BitValue Int Int
    | Store8BitValue Int Int


tripleMainRegs : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, TriplePCIncrement, InstructionDuration )
tripleMainRegs =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_hl, IncrementByThree, SixteenTStates ) )
        ]


tripleMainRegsIX : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, TriplePCIncrement, InstructionDuration )
tripleMainRegsIX =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_ix, IncrementByFour, TwentyTStates ) )
        , ( 0x36, ( ld_indirect_ix_n, IncrementByFour, TwentyTStates ) )
        ]


tripleMainRegsIY : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, TriplePCIncrement, InstructionDuration )
tripleMainRegsIY =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_iy, IncrementByFour, TwentyTStates ) )
        , ( 0x36, ( ld_indirect_iy_n, IncrementByFour, TwentyTStates ) )
        ]


applyTripleMainChange : CpuTimeCTime -> TriplePCIncrement -> TripleMainChange -> Z80Core -> Z80Core
applyTripleMainChange time pcInc z80changeData z80 =
    let
        env =
            z80.env

        new_pc =
            case pcInc of
                IncrementByThree ->
                    Bitwise.and (z80.pc + 3) 0xFFFF

                IncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF
    in
    case z80changeData of
        Store16BitValue address value ->
            let
                env1 =
                    { env | time = time } |> setMem16 address value
            in
            { z80
                | pc = new_pc
                , env = env1
                , r = z80.r + 1
            }

        Store8BitValue address value ->
            let
                env1 =
                    { env | time = time } |> setMem address value
            in
            { z80
                | pc = new_pc
                , env = env1
                , r = z80.r + 1
            }


ld_nn_indirect_hl : Int -> MainWithIndexRegisters -> TripleMainChange
ld_nn_indirect_hl param16 z80_main =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,HL); time+=6; break;
    Store16BitValue param16 z80_main.hl


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
