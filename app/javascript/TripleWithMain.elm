module TripleWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import RegisterChange exposing (DoubleWithRegisterChange(..))
import TripleByte exposing (TripleByteIndexChange(..))
import Utils exposing (shiftRightBy8)
import Z80Types exposing (MainWithIndexRegisters)


tripleMainRegsIXThree : Dict Int ( Int -> DoubleWithRegisterChange, InstructionDuration )
tripleMainRegsIXThree =
    Dict.fromList
        [ ( 0x70, ( \offset -> store_reg_indirect offset .ix .b, NineteenTStates ) )
        , ( 0x71, ( \param16 -> store_reg_indirect param16 .ix .c, NineteenTStates ) )
        , ( 0x72, ( \param16 -> store_reg_indirect param16 .ix .d, NineteenTStates ) )
        , ( 0x73, ( \param16 -> store_reg_indirect param16 .ix .e, NineteenTStates ) )
        , ( 0x74, ( \param16 -> store_reg_indirect param16 .ix (\z80_main -> z80_main.hl |> shiftRightBy8), NineteenTStates ) )
        , ( 0x75, ( \param16 -> store_reg_indirect param16 .ix (\z80_main -> z80_main.hl |> Bitwise.and 0xFF), NineteenTStates ) )
        ]


tripleMainRegsIXFour : Dict Int ( Int -> TripleByteIndexChange, InstructionDuration )
tripleMainRegsIXFour =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_ix, TwentyTStates ) )
        , ( 0x36, ( ld_indirect_ix_n, TwentyTStates ) )
        ]


tripleMainRegsIYThree : Dict Int ( Int -> DoubleWithRegisterChange, InstructionDuration )
tripleMainRegsIYThree =
    Dict.fromList
        [ ( 0x70, ( \offset -> store_reg_indirect offset .iy .b, NineteenTStates ) )
        , ( 0x71, ( \offset -> store_reg_indirect offset .iy .c, NineteenTStates ) )
        , ( 0x72, ( \param16 -> store_reg_indirect param16 .iy .d, NineteenTStates ) )
        , ( 0x73, ( \param16 -> store_reg_indirect param16 .iy .e, NineteenTStates ) )
        , ( 0x74, ( \param16 -> store_reg_indirect param16 .iy (\z80_main -> z80_main.hl |> shiftRightBy8), NineteenTStates ) )
        , ( 0x75, ( \param16 -> store_reg_indirect param16 .iy (\z80_main -> z80_main.hl |> Bitwise.and 0xFF), NineteenTStates ) )
        ]


tripleMainRegsIYFour : Dict Int ( Int -> TripleByteIndexChange, InstructionDuration )
tripleMainRegsIYFour =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_iy, TwentyTStates ) )
        , ( 0x36, ( ld_indirect_iy_n, TwentyTStates ) )
        ]


ld_nn_indirect_ix : Int -> TripleByteIndexChange
ld_nn_indirect_ix param16 =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,xy); time+=6; break;
    Store16BitValue param16 .ix


ld_nn_indirect_iy : Int -> TripleByteIndexChange
ld_nn_indirect_iy param16 =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,xy); time+=6; break;
    Store16BitValue param16 .iy


ld_indirect_ix_n : Int -> TripleByteIndexChange
ld_indirect_ix_n param16 =
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    let
        offset =
            param16 |> Bitwise.and 0xFF

        value =
            param16 |> shiftRightBy8
    in
    Store8BitValue offset .ix value


ld_indirect_iy_n : Int -> TripleByteIndexChange
ld_indirect_iy_n param16 =
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    let
        offset =
            param16 |> Bitwise.and 0xFF

        value =
            param16 |> shiftRightBy8
    in
    Store8BitValue offset .iy value


store_reg_indirect : Int -> (MainWithIndexRegisters -> Int) -> (MainWithIndexRegisters -> Int) -> DoubleWithRegisterChange
store_reg_indirect param source dest =
    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
    --let
    --    offset =
    --        param |> byte
    --in
    --RegStore8BitValue ((source + offset) |> Bitwise.and 0xFFFF) dest
    RegStore8BitValue param source dest
