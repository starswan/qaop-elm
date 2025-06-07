module TripleByte exposing (..)

import CpuTimeCTime exposing (InstructionDuration(..))
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..), TriplePCIncrement(..))
import Z80Word exposing (Z80Word)


type TripleByteChange
    = NewBCRegister Z80Word
    | NewDERegister Z80Word
    | NewHLRegister Z80Word
    | NewHLIndirect Z80Word
    | NewIXRegister Z80Word
    | NewIXIndirect Z80Word
    | NewIYRegister Z80Word
    | NewIYIndirect Z80Word
    | NewSPRegister Z80Word
    | NewPCRegister Z80Word
    | CallImmediate Z80Word


tripleByteWith16BitParam : Dict Int ( Z80Word -> TripleByteChange, TriplePCIncrement, InstructionDuration )
tripleByteWith16BitParam =
    Dict.fromList
        [ ( 0x01, ( ld_bc_nn, IncrementByThree, TenTStates ) )
        , ( 0x11, ( ld_de_nn, IncrementByThree, TenTStates ) )
        , ( 0x21, ( ld_hl_nn, IncrementByThree, TenTStates ) )
        , ( 0x2A, ( ld_hl_indirect_nn, IncrementByThree, SixteenTStates ) )
        , ( 0x31, ( ld_sp_nn, IncrementByThree, TenTStates ) )
        , ( 0xC3, ( jp_nn, IncrementByThree, TenTStates ) )
        , ( 0xCD, ( call_0xCD, IncrementByThree, SeventeenTStates ) )
        ]


tripleByteWith16BitParamDD : Dict Int ( Z80Word -> TripleByteChange, TriplePCIncrement, InstructionDuration )
tripleByteWith16BitParamDD =
    Dict.fromList
        [ ( 0x21, ( ld_ix_nn, IncrementByFour, TwentyTStates ) )
        , ( 0x2A, ( ld_ix_indirect_nn, IncrementByFour, TwentyTStates ) )
        ]


tripleByteWith16BitParamFD : Dict Int ( Z80Word -> TripleByteChange, TriplePCIncrement, InstructionDuration )
tripleByteWith16BitParamFD =
    Dict.fromList
        [ ( 0x21, ( ld_iy_nn, IncrementByFour, TwentyTStates ) )
        , ( 0x2A, ( ld_iy_indirect_nn, IncrementByFour, TwentyTStates ) )
        ]


ld_bc_nn : Z80Word -> TripleByteChange
ld_bc_nn param16 =
    -- case 0x01: v=imm16(); B=v>>>8; C=v&0xFF; break;
    NewBCRegister param16


ld_de_nn : Z80Word -> TripleByteChange
ld_de_nn param16 =
    --case 0x11: v=imm16(); D=v>>>8; E=v&0xFF; break;
    NewDERegister param16


ld_hl_nn : Z80Word -> TripleByteChange
ld_hl_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewHLRegister param16


ld_ix_nn : Z80Word -> TripleByteChange
ld_ix_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewIXRegister param16


ld_iy_nn : Z80Word -> TripleByteChange
ld_iy_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewIYRegister param16


ld_sp_nn : Z80Word -> TripleByteChange
ld_sp_nn param16 =
    -- case 0x31: SP=imm16(); break;
    NewSPRegister param16


jp_nn : Z80Word -> TripleByteChange
jp_nn param16 =
    -- case 0xC3: MP=PC=imm16(); break;
    NewPCRegister param16


call_0xCD : Z80Word -> TripleByteChange
call_0xCD param16 =
    -- case 0xCD: v=imm16(); push(PC); MP=PC=v; break;
    CallImmediate param16


ld_hl_indirect_nn : Z80Word -> TripleByteChange
ld_hl_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
    NewHLIndirect param16


ld_ix_indirect_nn : Z80Word -> TripleByteChange
ld_ix_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    NewIXIndirect param16


ld_iy_indirect_nn : Z80Word -> TripleByteChange
ld_iy_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    NewIYIndirect param16
