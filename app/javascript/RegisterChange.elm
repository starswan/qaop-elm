module RegisterChange exposing (..)

import Utils exposing (BitTest)
import Z80Core exposing (DirectionForLDIR)
import Z80Flags exposing (FlagFunc, FlagRegisters)
import Z80Registers exposing (ChangeMainRegister, ChangeOneRegister)
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
    = ChangeRegisterBC Int Int
    | ChangeRegisterDE Int Int
    | ChangeRegisterHL Int
    | ChangeRegisterIX Int
    | ChangeRegisterIY Int
    | ChangeRegisterIXH Int
    | ChangeRegisterIXL Int
    | ChangeRegisterIYH Int
    | ChangeRegisterIYL Int
    | PushedValue Int
    | RegChangeNewSP Int
    | IncrementIndirect Int
    | DecrementIndirect Int
    | RegisterChangeJump Int
    | SetIndirect Int Int
    | ChangeRegisterDEAndHL Int Int
    | RegisterChangeShifter Shifter Int
    | RegisterChangeIndexShifter Shifter Int
    | IndirectBitReset BitTest Int
    | IndirectBitSet BitTest Int
    | RegChangeNoOp
    | SingleEnvFlagFunc FlagFunc Int
    | ExchangeTopOfStackWith IXIYHL
    | SingleRegisterChange ChangeOneRegister Int
    | RegisterIndirectWithShifter Shifter ChangeMainRegister Int
    | SetBitIndirectWithCopy BitTest ChangeMainRegister Int
    | ResetBitIndirectWithCopy BitTest ChangeMainRegister Int


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


type InterruptChange
    = LoadAFromIR Int
