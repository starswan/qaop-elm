module TripleByte exposing (..)

import CpuTimeCTime exposing (InstructionDuration(..))
import Dict exposing (Dict)


type TripleByteRegister
    = TripleByteBC
    | TripleByteDE
    | TripleByteHL


type TripleByteChange
    = NewHLIndirect Int
    | NewIXRegister Int
    | NewIXIndirect Int
    | NewIYRegister Int
    | NewIYIndirect Int
    | NewSPRegister Int
    | NewAIndirect Int
    | NewTripleRegister Int TripleByteRegister
    | TripleSetIndirectFromA Int


tripleByteWith16BitParam : Dict Int ( Int -> TripleByteChange, InstructionDuration )
tripleByteWith16BitParam =
    Dict.fromList
        [ ( 0x01, ( ld_bc_nn, TenTStates ) )
        , ( 0x11, ( ld_de_nn, TenTStates ) )
        , ( 0x21, ( ld_hl_nn, TenTStates ) )
        , ( 0x2A, ( ld_hl_indirect_nn, SixteenTStates ) )
        , ( 0x31, ( ld_sp_nn, TenTStates ) )
        , ( 0x32, ( ld_indirect_nn_a, ThirteenTStates ) )
        , ( 0x3A, ( ld_a_indirect_nn, ThirteenTStates ) )
        ]


tripleByteWith16BitParamDD : Dict Int ( Int -> TripleByteChange, InstructionDuration )
tripleByteWith16BitParamDD =
    Dict.fromList
        [ ( 0x21, ( ld_ix_nn, TwentyTStates ) )
        , ( 0x2A, ( ld_ix_indirect_nn, TwentyTStates ) )
        ]


tripleByteWith16BitParamFD : Dict Int ( Int -> TripleByteChange, InstructionDuration )
tripleByteWith16BitParamFD =
    Dict.fromList
        [ ( 0x21, ( ld_iy_nn, TwentyTStates ) )
        , ( 0x2A, ( ld_iy_indirect_nn, TwentyTStates ) )
        ]


ld_bc_nn : Int -> TripleByteChange
ld_bc_nn param16 =
    -- case 0x01: v=imm16(); B=v>>>8; C=v&0xFF; break;
    NewTripleRegister param16 TripleByteBC


ld_de_nn : Int -> TripleByteChange
ld_de_nn param16 =
    --case 0x11: v=imm16(); D=v>>>8; E=v&0xFF; break;
    NewTripleRegister param16 TripleByteDE


ld_hl_nn : Int -> TripleByteChange
ld_hl_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewTripleRegister param16 TripleByteHL


ld_ix_nn : Int -> TripleByteChange
ld_ix_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewIXRegister param16


ld_iy_nn : Int -> TripleByteChange
ld_iy_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewIYRegister param16


ld_sp_nn : Int -> TripleByteChange
ld_sp_nn param16 =
    -- case 0x31: SP=imm16(); break;
    NewSPRegister param16


ld_hl_indirect_nn : Int -> TripleByteChange
ld_hl_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
    NewHLIndirect param16


ld_ix_indirect_nn : Int -> TripleByteChange
ld_ix_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    NewIXIndirect param16


ld_iy_indirect_nn : Int -> TripleByteChange
ld_iy_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    NewIYIndirect param16


ld_a_indirect_nn : Int -> TripleByteChange
ld_a_indirect_nn param16 =
    -- case 0x3A: MP=(v=imm16())+1; A=env.mem(v); time+=3; break;
    NewAIndirect param16


ld_indirect_nn_a : Int -> TripleByteChange
ld_indirect_nn_a param =
    -- case 0x32: MP=(v=imm16())+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    TripleSetIndirectFromA param
