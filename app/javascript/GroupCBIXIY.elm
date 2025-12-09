module GroupCBIXIY exposing (..)

import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..))
import Dict exposing (Dict)
import RegisterChange exposing (RegisterChange(..), Shifter(..))
import SingleEnvWithMain exposing (SingleEnvMainChange(..))
import Utils exposing (BitTest(..), byte)
import Z80Env exposing (Z80Env)
import Z80Registers exposing (ChangeMainRegister(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


singleByteMainRegsIXCB : Dict Int ( Int -> MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsIXCB =
    Dict.fromList
        [ --shifter0
          ( 0x00, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x01, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x02, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x03, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x04, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x05, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x06, ( \offset z80_main -> RegisterChangeIndexShifter Shifter0 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        --shifter1
        , ( 0x08, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x09, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x0A, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x0B, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x0C, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x0D, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x0E, ( \offset z80_main -> RegisterChangeIndexShifter Shifter1 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        --shifter2
        , ( 0x10, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x11, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x12, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x13, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x14, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x15, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x16, ( \offset z80_main -> RegisterChangeIndexShifter Shifter2 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        --shifter3
        , ( 0x18, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x19, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x1A, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x1B, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x1C, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x1D, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x1E, ( \offset z80_main -> RegisterChangeIndexShifter Shifter3 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        --shifter4
        , ( 0x20, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x21, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x22, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x23, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x24, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x25, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x26, ( \offset z80_main -> RegisterChangeIndexShifter Shifter4 (z80_main.ix + byte offset), FifteenTStates ) )

        --shifter5
        , ( 0x28, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x29, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x2A, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x2B, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x2C, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x2D, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x2E, ( \offset z80_main -> RegisterChangeIndexShifter Shifter5 (z80_main.ix + byte offset), FifteenTStates ) )

        --shifter6
        , ( 0x30, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x31, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x32, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x33, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x34, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x35, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x36, ( \offset z80_main -> RegisterChangeIndexShifter Shifter6 (z80_main.ix + byte offset), FifteenTStates ) )

        --shifter7
        , ( 0x38, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x39, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x3A, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x3B, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x3C, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x3D, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x3E, ( \offset z80_main -> RegisterChangeIndexShifter Shifter7 (z80_main.ix + byte offset), FifteenTStates ) )

        -- reset bit0
        , ( 0x80, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x81, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x82, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x83, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x84, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x85, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x86, ( resetIXbit Bit_0, TwentyThreeTStates ) )

        -- reset bit1
        , ( 0x88, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x89, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x8A, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x8B, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x8C, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x8D, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x8E, ( resetIXbit Bit_1, TwentyThreeTStates ) )

        -- reset bit2
        , ( 0x90, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x91, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x92, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x93, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x94, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x95, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x96, ( resetIXbit Bit_2, TwentyThreeTStates ) )

        -- reset bit3
        , ( 0x98, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x99, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x9A, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x9B, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x9C, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x9D, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0x9E, ( resetIXbit Bit_3, TwentyThreeTStates ) )

        -- reset bit4
        , ( 0xA0, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xA1, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xA2, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xA3, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xA4, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xA5, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xA6, ( resetIXbit Bit_4, TwentyThreeTStates ) )

        -- reset bit5
        , ( 0xA8, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xA9, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xAA, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xAB, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xAC, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xAD, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xAE, ( resetIXbit Bit_5, TwentyThreeTStates ) )

        -- reset bit6
        , ( 0xB0, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xB1, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xB2, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xB3, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xB4, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xB5, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xB6, ( resetIXbit Bit_6, TwentyThreeTStates ) )

        -- reset bit7
        , ( 0xB8, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xB9, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xBA, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xBB, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xBC, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xBD, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xBE, ( resetIXbit Bit_7, TwentyThreeTStates ) )

        -- set bit0
        , ( 0xC0, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xC1, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xC2, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xC3, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xC4, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xC5, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xC6, ( \offset z80_main -> IndirectBitSet Bit_0 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        -- set bit1
        , ( 0xC8, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xC9, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xCA, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xCB, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xCC, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xCD, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xCE, ( \offset z80_main -> IndirectBitSet Bit_1 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        -- set bit2
        , ( 0xD0, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xD1, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xD2, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xD3, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xD4, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xD5, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xD6, ( \offset z80_main -> IndirectBitSet Bit_2 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        -- set bit3
        , ( 0xD8, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xD9, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xDA, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xDB, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xDC, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xDD, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xDE, ( \offset z80_main -> IndirectBitSet Bit_3 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        -- set bit4
        , ( 0xE0, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xE1, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xE2, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xE3, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xE4, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xE5, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xE6, ( \offset z80_main -> IndirectBitSet Bit_4 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        -- set bit5
        , ( 0xE8, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xE9, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xEA, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xEB, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xEC, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xED, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xEE, ( \offset z80_main -> IndirectBitSet Bit_5 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        -- set bit6
        , ( 0xF0, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xF1, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xF2, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xF3, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xF4, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xF5, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xF6, ( \offset z80_main -> IndirectBitSet Bit_6 (z80_main.ix + byte offset), TwentyThreeTStates ) )

        -- set bit7
        , ( 0xF8, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xF9, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xFA, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xFB, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xFC, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xFD, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates ) )
        , ( 0xFE, ( \offset z80_main -> IndirectBitSet Bit_7 (z80_main.ix + byte offset), TwentyThreeTStates ) )
        ]


singleByteMainRegsIYCB : Dict Int ( Int -> MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsIYCB =
    Dict.fromList
        [ --shifter0
          ( 0x00, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x01, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x02, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x03, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x04, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x05, ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x06, ( \offset z80_main -> RegisterChangeIndexShifter Shifter0 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        --shifter1
        , ( 0x08, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x09, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x0A, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x0B, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x0C, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x0D, ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x0E, ( \offset z80_main -> RegisterChangeIndexShifter Shifter1 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        --shifter2
        , ( 0x10, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x11, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x12, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x13, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x14, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x15, ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x16, ( \offset z80_main -> RegisterChangeIndexShifter Shifter2 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        --shifter3
        , ( 0x18, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x19, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x1A, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x1B, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x1C, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x1D, ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x1E, ( \offset z80_main -> RegisterChangeIndexShifter Shifter3 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        --shifter4
        , ( 0x20, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x21, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x22, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x23, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x24, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x25, ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x26, ( \offset z80_main -> RegisterChangeIndexShifter Shifter4 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        --shifter5
        , ( 0x28, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x29, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x2A, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x2B, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x2C, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x2D, ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x2E, ( \offset z80_main -> RegisterChangeIndexShifter Shifter5 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        --shifter6
        , ( 0x30, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x31, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x32, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x33, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x34, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x35, ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x36, ( \offset z80_main -> RegisterChangeIndexShifter Shifter6 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        --shifter7
        , ( 0x38, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x39, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x3A, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x3B, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x3C, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x3D, ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x3E, ( \offset z80_main -> RegisterChangeIndexShifter Shifter7 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        -- reset bit0
        , ( 0x80, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x81, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x82, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x83, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x84, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x85, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x86, ( resetIYbit Bit_0, TwentyThreeTStates ) )

        -- reset bit1
        , ( 0x88, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x89, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x8A, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x8B, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x8C, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x8D, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x8E, ( resetIYbit Bit_1, TwentyThreeTStates ) )

        -- reset bit2
        , ( 0x90, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x91, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x92, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x93, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x94, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x95, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x96, ( resetIYbit Bit_2, TwentyThreeTStates ) )

        -- reset bit3
        , ( 0x98, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x99, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x9A, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x9B, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x9C, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x9D, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0x9E, ( resetIYbit Bit_3, TwentyThreeTStates ) )

        -- reset bit4
        , ( 0xA0, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xA1, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xA2, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xA3, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xA4, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xA5, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xA6, ( resetIYbit Bit_4, TwentyThreeTStates ) )

        -- reset bit5
        , ( 0xA8, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xA9, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xAA, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xAB, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xAC, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xAD, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xAE, ( resetIYbit Bit_5, TwentyThreeTStates ) )

        -- reset bit6
        , ( 0xB0, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xB1, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xB2, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xB3, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xB4, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xB5, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xB6, ( resetIYbit Bit_6, TwentyThreeTStates ) )

        -- reset bit7
        , ( 0xB8, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xB9, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xBA, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xBB, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xBC, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xBD, ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xBE, ( resetIYbit Bit_7, TwentyThreeTStates ) )

        -- set bit0
        , ( 0xC0, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xC1, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xC2, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xC3, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xC4, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xC5, ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xC6, ( \offset z80_main -> IndirectBitSet Bit_0 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        -- set bit1
        , ( 0xC8, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xC9, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xCA, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xCB, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xCC, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xCD, ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xCE, ( \offset z80_main -> IndirectBitSet Bit_1 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        -- set bit2
        , ( 0xD0, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xD1, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xD2, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xD3, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xD4, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xD5, ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xD6, ( \offset z80_main -> IndirectBitSet Bit_2 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        -- set bit3
        , ( 0xD8, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xD9, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xDA, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xDB, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xDC, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xDD, ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xDE, ( \offset z80_main -> IndirectBitSet Bit_3 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        -- set bit4
        , ( 0xE0, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xE1, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xE2, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xE3, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xE4, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xE5, ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xE6, ( \offset z80_main -> IndirectBitSet Bit_4 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        -- set bit5
        , ( 0xE8, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xE9, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xEA, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xEB, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xEC, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xED, ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xEE, ( \offset z80_main -> IndirectBitSet Bit_5 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        -- set bit6
        , ( 0xF0, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xF1, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xF2, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xF3, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xF4, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xF5, ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xF6, ( \offset z80_main -> IndirectBitSet Bit_6 (z80_main.iy + byte offset), TwentyThreeTStates ) )

        -- set bit7
        , ( 0xF8, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xF9, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xFA, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xFB, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xFC, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xFD, ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates ) )
        , ( 0xFE, ( \offset z80_main -> IndirectBitSet Bit_7 (z80_main.iy + byte offset), TwentyThreeTStates ) )
        ]


bitTests =
    [ Bit_0, Bit_1, Bit_2, Bit_3, Bit_4, Bit_5, Bit_6, Bit_7 ]


makeEnvMainDict : (MainWithIndexRegisters -> Int) -> Dict Int ( MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
makeEnvMainDict ix_func =
    bitTests
        |> List.indexedMap
            (\bitIndex bitType ->
                let
                    start =
                        0x40 + bitIndex * 8
                in
                List.range start (start + 7)
                    |> List.map
                        (\index ->
                            ( index
                            , ( \z80_main offset rom48k z80_env ->
                                    let
                                        --int a = mp = (char)(xy + (byte)env.mem(pc));
                                        --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
                                        address =
                                            (ix_func z80_main + byte offset) |> Bitwise.and 0xFFFF
                                    in
                                    IndirectBitTest bitType address
                              , TwentyTStates
                              )
                            )
                        )
                    |> Dict.fromList
            )
        |> List.foldr (\d1 d2 -> d1 |> Dict.union d2) Dict.empty


singleEnvMainRegsIXCB : Dict Int ( MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsIXCB =
    makeEnvMainDict (\z80_main -> z80_main.ix)


singleEnvMainRegsIYCB : Dict Int ( MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsIYCB =
    makeEnvMainDict (\z80_main -> z80_main.iy)


resetIXbit : BitTest -> Int -> MainWithIndexRegisters -> RegisterChange
resetIXbit bitMask offset z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF)


resetIYbit : BitTest -> Int -> MainWithIndexRegisters -> RegisterChange
resetIYbit bitMask offset z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF)
