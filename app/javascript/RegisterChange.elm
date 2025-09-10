module RegisterChange exposing (..)

import Utils exposing (BitTest)
import Z80Flags exposing (FlagFunc, FlagRegisters)
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


type ChangeOneRegister
    = ChangeARegister
    | ChangeBRegister
    | ChangeCRegister
    | ChangeDRegister
    | ChangeERegister
    | ChangeHRegister
    | ChangeLRegister


type ChangeMainRegister
    = ChangeMainB
    | ChangeMainC
    | ChangeMainD
    | ChangeMainE
    | ChangeMainH
    | ChangeMainL


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
    | RegChangeIm InterruptMode
    | ExchangeTopOfStackWith IXIYHL
    | SingleRegisterChange ChangeOneRegister Int
    | RegisterIndirectWithShifter Shifter ChangeMainRegister Int
    | SetBitIndirectWithCopy BitTest ChangeMainRegister Int
    | ResetBitIndirectWithCopy BitTest ChangeMainRegister Int
    | LoadAFromIR Int
