module GroupCBIXIY exposing (..)

import Array exposing (Array)
import Bitwise
import IXIYChange exposing (IXIYChange(..))
import InstructionDuration exposing (InstructionDuration(..))
import Utils exposing (BitTest(..), byte)
import Z80Change exposing (Shifter(..))
import Z80Registers exposing (ChangeMainRegister(..))
import Z80Types exposing (MainWithIndexRegisters)


singleByteMainRegsIXCB : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsIXCB =
    Array.fromList
        [ --shifter0
          ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter0 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter0 (z80_main.ix + byte offset), NumberedTStates 23 )

        --1
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter1 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter1 (z80_main.ix + byte offset), NumberedTStates 23 )

        --2
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter2 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter2 (z80_main.ix + byte offset), NumberedTStates 23 )

        --3
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter3 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter3 (z80_main.ix + byte offset), NumberedTStates 23 )

        --4
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter4 (z80_main.ix + byte offset), NumberedTStates 15 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter4 (z80_main.ix + byte offset), NumberedTStates 23 )

        --5
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter5 (z80_main.ix + byte offset), NumberedTStates 15 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter5 (z80_main.ix + byte offset), NumberedTStates 23 )

        --6
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter6 (z80_main.ix + byte offset), NumberedTStates 15 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter6 (z80_main.ix + byte offset), NumberedTStates 23 )

        --7
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter7 (z80_main.ix + byte offset), NumberedTStates 15 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter7 (z80_main.ix + byte offset), NumberedTStates 23 )
        ]


singleByteMainRegsIXCB80 : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsIXCB80 =
    Array.fromList
        [ -- reset bit0
          ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( resetIXbit Bit_0, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_0 (z80_main.ix + byte offset), NumberedTStates 23 )

        --bit1
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( resetIXbit Bit_1, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_1 (z80_main.ix + byte offset), NumberedTStates 23 )

        --bit2
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( resetIXbit Bit_2, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_2 (z80_main.ix + byte offset), NumberedTStates 23 )

        --bit3
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( resetIXbit Bit_3, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_3 (z80_main.ix + byte offset), NumberedTStates 23 )

        --bit4
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( resetIXbit Bit_4, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_4 (z80_main.ix + byte offset), NumberedTStates 23 )

        --bit5
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( resetIXbit Bit_5, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_5 (z80_main.ix + byte offset), NumberedTStates 23 )

        --bit6
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( resetIXbit Bit_6, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_6 (z80_main.ix + byte offset), NumberedTStates 23 )

        --bit7
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( resetIXbit Bit_7, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_7 (z80_main.ix + byte offset), NumberedTStates 23 )

        --t0
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_0 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_0 (z80_main.ix + byte offset), NumberedTStates 23 )

        --t1
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_1 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_1 (z80_main.ix + byte offset), NumberedTStates 23 )

        --t2
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_2 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_2 (z80_main.ix + byte offset), NumberedTStates 23 )

        --t3
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_3 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_3 (z80_main.ix + byte offset), NumberedTStates 23 )

        --t4
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_4 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_4 (z80_main.ix + byte offset), NumberedTStates 23 )

        --t5
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_5 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_5 (z80_main.ix + byte offset), NumberedTStates 23 )

        --t6
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_6 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_6 (z80_main.ix + byte offset), NumberedTStates 23 )

        --t7
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_7 (z80_main.ix + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_7 (z80_main.ix + byte offset), NumberedTStates 23 )
        ]


singleByteMainRegsIYCB : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsIYCB =
    Array.fromList
        [ --shifter0 0x00 - 0x07
          ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter0 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter0 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter0 (z80_main.iy + byte offset), NumberedTStates 23 )

        --shifter1
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter1 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter1 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter1 (z80_main.iy + byte offset), NumberedTStates 23 )

        --shifter2
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter2 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter2 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter2 (z80_main.iy + byte offset), NumberedTStates 23 )

        --shifter3
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter3 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter3 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter3 (z80_main.iy + byte offset), NumberedTStates 23 )

        --shifter4
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter4 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter4 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter4 (z80_main.iy + byte offset), NumberedTStates 23 )

        --shifter5
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter5 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter5 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter5 (z80_main.iy + byte offset), NumberedTStates 23 )

        --shifter6
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter6 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter6 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter6 (z80_main.iy + byte offset), NumberedTStates 23 )

        --shifter7 0x38 - 0x3F
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterIndirectWithShifter Shifter7 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> RegisterChangeIndexShifter Shifter7 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> FlagsIndirectWithShifter Shifter7 (z80_main.iy + byte offset), NumberedTStates 23 )
        ]


singleByteMainRegsIYCB80 : Array ( Int -> MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsIYCB80 =
    Array.fromList
        -- reset bit0
        [ ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( resetIYbit Bit_0, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_0 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- reset bit1
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( resetIYbit Bit_1, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_1 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- reset bit2
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( resetIYbit Bit_2, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_2 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- reset bit3
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( resetIYbit Bit_3, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_3 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- reset bit4
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( resetIYbit Bit_4, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_4 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- reset bit5
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( resetIYbit Bit_5, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_5 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- reset bit6
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( resetIYbit Bit_6, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_6 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- reset bit7
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( resetIYbit Bit_7, NumberedTStates 23 )
        , ( \offset z80_main -> ResetBitIndirectA Bit_7 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- set bit0
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_0 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_0 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_0 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- set bit1
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_1 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_1 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_1 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- set bit2
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_2 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_2 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_2 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- set bit3
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_3 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_3 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_3 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- set bit4
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_4 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_4 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_4 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- set bit5
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_5 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_5 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_5 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- set bit6
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_6 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_6 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_6 (z80_main.iy + byte offset), NumberedTStates 23 )

        -- set bit7
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainB (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainC (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainD (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainE (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainH (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectWithCopy Bit_7 ChangeMainL (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> IndirectBitSet Bit_7 (z80_main.iy + byte offset), NumberedTStates 23 )
        , ( \offset z80_main -> SetBitIndirectA Bit_7 (z80_main.iy + byte offset), NumberedTStates 23 )
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
                                      , NumberedTStates 20
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
