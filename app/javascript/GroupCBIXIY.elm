module GroupCBIXIY exposing (..)

import Array exposing (Array)
import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..))
import IXIYChange exposing (IXIYChange(..))
import Utils exposing (BitTest(..), byte)
import Z80Change exposing (Shifter(..))
import Z80Registers exposing (ChangeMainRegister(..))
import Z80Types exposing (MainWithIndexRegisters)


singleByteMainRegsIXCB : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsIXCB =
    Array.fromList
        [ --shifter0
          ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter0 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter0 (z80_main.ix + byte offset), TwentyThreeTStates )

        --1
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter1 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter1 (z80_main.ix + byte offset), TwentyThreeTStates )

        --2
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter2 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter2 (z80_main.ix + byte offset), TwentyThreeTStates )

        --3
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter3 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter3 (z80_main.ix + byte offset), TwentyThreeTStates )

        --4
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter4 (z80_main.ix + byte offset), FifteenTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter4 (z80_main.ix + byte offset), TwentyThreeTStates )

        --5
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter5 (z80_main.ix + byte offset), FifteenTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter5 (z80_main.ix + byte offset), TwentyThreeTStates )

        --6
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter6 (z80_main.ix + byte offset), FifteenTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter6 (z80_main.ix + byte offset), TwentyThreeTStates )

        --7
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter7 (z80_main.ix + byte offset), FifteenTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter7 (z80_main.ix + byte offset), TwentyThreeTStates )
        ]


singleByteMainRegsIXCB80 : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsIXCB80 =
    Array.fromList
        [ -- reset bit0
          ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( resetIXbit Bit_0, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_0 (z80_main.ix + byte offset), TwentyThreeTStates )

        --bit1
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( resetIXbit Bit_1, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_1 (z80_main.ix + byte offset), TwentyThreeTStates )

        --bit2
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( resetIXbit Bit_2, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_2 (z80_main.ix + byte offset), TwentyThreeTStates )

        --bit3
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( resetIXbit Bit_3, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_3 (z80_main.ix + byte offset), TwentyThreeTStates )

        --bit4
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( resetIXbit Bit_4, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_4 (z80_main.ix + byte offset), TwentyThreeTStates )

        --bit5
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( resetIXbit Bit_5, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_5 (z80_main.ix + byte offset), TwentyThreeTStates )

        --bit6
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( resetIXbit Bit_6, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_6 (z80_main.ix + byte offset), TwentyThreeTStates )

        --bit7
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( resetIXbit Bit_7, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_7 (z80_main.ix + byte offset), TwentyThreeTStates )

        --t0
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_0 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_0 (z80_main.ix + byte offset), TwentyThreeTStates )

        --t1
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_1 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_1 (z80_main.ix + byte offset), TwentyThreeTStates )

        --t2
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_2 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_2 (z80_main.ix + byte offset), TwentyThreeTStates )

        --t3
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_3 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_3 (z80_main.ix + byte offset), TwentyThreeTStates )

        --t4
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_4 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_4 (z80_main.ix + byte offset), TwentyThreeTStates )

        --t5
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_5 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_5 (z80_main.ix + byte offset), TwentyThreeTStates )

        --t6
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_6 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_6 (z80_main.ix + byte offset), TwentyThreeTStates )

        --t7
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_7 (z80_main.ix + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_7 (z80_main.ix + byte offset), TwentyThreeTStates )
        ]


singleByteMainRegsIYCB : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsIYCB =
    Array.fromList
        [ --shifter0 0x00 - 0x07
          ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter0 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter0 (z80_main.iy + byte offset), TwentyThreeTStates )

        --shifter1
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter1 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter1 (z80_main.iy + byte offset), TwentyThreeTStates )

        --shifter2
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter2 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter2 (z80_main.iy + byte offset), TwentyThreeTStates )

        --shifter3
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter3 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter3 (z80_main.iy + byte offset), TwentyThreeTStates )

        --shifter4
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter4 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter4 (z80_main.iy + byte offset), TwentyThreeTStates )

        --shifter5
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter5 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter5 (z80_main.iy + byte offset), TwentyThreeTStates )

        --shifter6
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter6 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter6 (z80_main.iy + byte offset), TwentyThreeTStates )

        --shifter7 0x38 - 0x3F
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter7 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter7 (z80_main.iy + byte offset), TwentyThreeTStates )
        ]


singleByteMainRegsIYCB80 : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsIYCB80 =
    Array.fromList
        -- reset bit0
        [ ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( resetIYbit Bit_0, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_0 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- reset bit1
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( resetIYbit Bit_1, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_1 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- reset bit2
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( resetIYbit Bit_2, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_2 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- reset bit3
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( resetIYbit Bit_3, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_3 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- reset bit4
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( resetIYbit Bit_4, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_4 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- reset bit5
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( resetIYbit Bit_5, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_5 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- reset bit6
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( resetIYbit Bit_6, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_6 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- reset bit7
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( resetIYbit Bit_7, TwentyThreeTStates )
        , ( \offset z80_main -> ResetBitIndirectA Bit_7 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- set bit0
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_0 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_0 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- set bit1
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_1 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_1 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- set bit2
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_2 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_2 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- set bit3
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_3 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_3 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- set bit4
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_4 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_4 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- set bit5
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_5 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_5 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- set bit6
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_6 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_6 (z80_main.iy + byte offset), TwentyThreeTStates )

        -- set bit7
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> IndirectBitSet Bit_7 (z80_main.iy + byte offset), TwentyThreeTStates )
        , ( \offset z80_main -> SetBitIndirectA Bit_7 (z80_main.iy + byte offset), TwentyThreeTStates )
        ]


bitTests =
    [ Bit_0, Bit_1, Bit_2, Bit_3, Bit_4, Bit_5, Bit_6, Bit_7 ]


makeEnvMainDict : (MainWithIndexRegisters -> Int) -> Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
makeEnvMainDict ix_func =
    let
        dictList : List (Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration ))
        dictList =
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
                                    , ( \offset z80_main ->
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
                            |> List.map Tuple.second
                            |> Array.fromList
                    )
    in
    --dictList |> List.foldr (\d1 d2 -> d1 |> Dict.union d2) Dict.empty
    dictList |> List.foldl (\d1 d2 -> d1 |> Array.append d2) Array.empty


singleEnvMainRegsIXCB40 : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleEnvMainRegsIXCB40 =
    makeEnvMainDict .ix


singleEnvMainRegsIYCB40 : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleEnvMainRegsIYCB40 =
    makeEnvMainDict .iy


resetIXbit : BitTest -> Int -> MainWithIndexRegisters -> IXIYChange
resetIXbit bitMask offset z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF)


resetIYbit : BitTest -> Int -> MainWithIndexRegisters -> IXIYChange
resetIYbit bitMask offset z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF)
