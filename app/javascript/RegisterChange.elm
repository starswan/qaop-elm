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
    = AlterRegisterA
    | AlterRegisterB
    | AlterRegisterC
    | AlterRegisterD
    | ChangeRegisterE
    | ChangeRegisterH
    | ChangeRegisterL


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
    | IndirectBitReset BitTest Int
    | IndirectBitSet BitTest Int
    | RegChangeNoOp
    | SingleEnvFlagFunc FlagFunc Int
    | RegChangeIm InterruptMode
    | ExchangeTopOfStackWith IXIYHL
    | SingleRegisterChange ChangeOneRegister Int
