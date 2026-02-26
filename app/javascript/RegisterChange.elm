module RegisterChange exposing (..)

import Utils exposing (BitTest)
import Z80Core exposing (DirectionForLDIR)
import Z80Flags exposing (FlagFunc)
import Z80Registers exposing (ChangeMainRegister, ChangeSingle)
import Z80Types exposing (IXIYHL, InterruptMode, MainWithIndexRegisters)


type Shifter
    = Shifter0
    | Shifter1
    | Shifter2
    | Shifter3
    | Shifter4
    | Shifter5
    | Shifter6
    | Shifter7


type RegisterChange
    = ChangeRegisterIYH Int
    | ChangeRegisterIYL Int
    | PushedValue (MainWithIndexRegisters -> Int)
    | RegChangeNewSP (MainWithIndexRegisters -> Int)
    | IncrementIndirect (MainWithIndexRegisters -> Int)
    | DecrementIndirect (MainWithIndexRegisters -> Int)
    | RegisterChangeJump (MainWithIndexRegisters -> Int)
    | SetIndirect (MainWithIndexRegisters -> Int) (MainWithIndexRegisters -> Int)
    | RegisterChangeShifter Shifter Int
    | RegisterChangeIndexShifter Shifter Int
    | IndirectBitReset BitTest Int
    | IndirectBitSet BitTest Int
    | RegChangeNoOp
    | SingleEnvFlagFunc FlagFunc (MainWithIndexRegisters -> Int)
    | ExchangeTopOfStackWith IXIYHL
    | SingleRegisterChange ChangeSingle Int
    | RegisterChangeA (MainWithIndexRegisters -> Int)
    | RegisterIndirectWithShifter Shifter ChangeMainRegister Int
    | SetBitIndirectWithCopy BitTest ChangeMainRegister Int
    | ResetBitIndirectWithCopy BitTest ChangeMainRegister Int
    | FlagsIndirectWithShifter Shifter Int
    | SetBitIndirectA BitTest Int
    | ResetBitIndirectA BitTest Int
    | TransformMainRegisters (MainWithIndexRegisters -> MainWithIndexRegisters)


type SixteenBit
    = RegHL
    | RegDE
    | RegBC
    | RegSP


type EDRegisterChange
    = EDNoOp
    | RegChangeIm InterruptMode
    | Z80InI DirectionForLDIR Bool
    | Z80OutI DirectionForLDIR Bool
    | InRC ChangeMainRegister
    | Ldir DirectionForLDIR Bool
    | Cpir DirectionForLDIR Bool
    | SbcHL SixteenBit
    | RRD
    | RLD
    | IN_C
    | IN_A_C
    | AdcHLSP


type EDFourByteChange
    = SetMemFrom Int SixteenBit
    | GetFromMem Int SixteenBit


type InterruptChange
    = LoadAFromIR Int
