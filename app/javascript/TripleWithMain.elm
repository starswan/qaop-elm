module TripleWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime)
import Dict exposing (Dict)
import Z80Env exposing (mem16)
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeEnvOperation(..), InstructionDuration(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (MainWithIndexRegisters, Z80)


type TripleMainChange
    = Store16BitValue Int Int


tripleMainRegs : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, InstructionLength )
tripleMainRegs =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_hl, ThreeByteInstruction ) )
        , ( 0xDD22, ( ld_nn_indirect_ix, FourByteInstruction ) )
        , ( 0xFD22, ( ld_nn_indirect_iy, FourByteInstruction ) )
        ]


parseTripleMain : Int ->  Int -> Z80ROM -> Z80 -> Maybe Z80Transform
parseTripleMain paramOffset instrCode rom48k  z80 =
    case tripleMainRegs |> Dict.get instrCode of
        Just ( f, new_pc ) ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
            in
            case f doubleParam.value z80.main of
                Store16BitValue addr data ->
                    Just
                        { pcIncrement = new_pc
                        , time = doubleParam.time
                        , timeIncrement = SixTStates
                        , operation = ChangeEnv (Store16BitMemoryValue addr data)
                        }

        Nothing ->
            Nothing


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
