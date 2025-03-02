module TripleWithMain exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)
import Dict exposing (Dict)
import PCIncrement exposing (TriplePCIncrement(..))
import Z80Address exposing (Z80Address, incrementBy3, incrementBy4, toInt)
import Z80Env exposing (Z80Env, setMem)
import Z80Types exposing (MainWithIndexRegisters, Z80)


type TripleMainChange
    = Store16BitValue Int Int


tripleMainRegs : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, TriplePCIncrement )
tripleMainRegs =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_hl, IncrementByThree ) )
        , ( 0xDD22, ( ld_nn_indirect_ix, IncrementByFour ) )
        , ( 0xFD22, ( ld_nn_indirect_iy, IncrementByFour ) )
        ]


applyTripleMainChange : CpuTimeCTime -> TriplePCIncrement -> TripleMainChange -> Z80 -> Z80
applyTripleMainChange time pcInc z80changeData z80 =
    let
        interrupts =
            z80.interrupts

        env =
            z80.env

        new_pc =
            case pcInc of
                IncrementByThree ->
                    z80.pc |> incrementBy3

                IncrementByFour ->
                    z80.pc |> incrementBy4
    in
    case z80changeData of
        Store16BitValue address value ->
            let
                env1 =
                    { env | time = time } |> setMem address value
            in
            { z80
                | pc = new_pc
                , env = env1
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }


ld_nn_indirect_hl : Int -> MainWithIndexRegisters -> TripleMainChange
ld_nn_indirect_hl param16 z80_main =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,HL); time+=6; break;
    Store16BitValue param16 (z80_main.hl |> toInt)


ld_nn_indirect_ix : Int -> MainWithIndexRegisters -> TripleMainChange
ld_nn_indirect_ix param16 z80_main =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,xy); time+=6; break;
    Store16BitValue param16 (z80_main.ix |> toInt)


ld_nn_indirect_iy : Int -> MainWithIndexRegisters -> TripleMainChange
ld_nn_indirect_iy param16 z80_main =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,xy); time+=6; break;
    Store16BitValue param16 (z80_main.iy |> toInt)
