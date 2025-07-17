module RegisterChange exposing (..)

import Bitwise
import Utils exposing (BitTest, shiftLeftBy8)
import Z80Flags exposing (FlagFunc, FlagRegisters, changeFlags)
import Z80Types exposing (IXIYHL, InterruptMode, MainWithIndexRegisters, set_de_main)


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
    | ChangeRegisterB Int
    | ChangeRegisterDE Int Int
    | ChangeRegisterE Int
    | ChangeRegisterHL Int
    | ChangeRegisterIX Int
    | ChangeRegisterIY Int
    | ChangeRegisterD Int
    | ChangeRegisterA Int
    | ChangeRegisterC Int
    | ChangeRegisterH Int
    | ChangeRegisterL Int
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


type RegisterChangeApplied
    = MainRegsApplied MainWithIndexRegisters
    | FlagRegsApplied FlagRegisters
    | PushedValueApplied Int
    | NewSPApplied Int
    | IncrementIndirectApplied Int
    | DecrementIndirectApplied Int
    | JumpApplied Int
    | SetIndirectApplied Int Int
    | RegisterChangeShifterApplied Shifter Int
    | IndirectBitResetApplied BitTest Int
    | IndirectBitSetApplied BitTest Int
    | RegChangeAppliedNoOp
    | RegChangeImApplied InterruptMode
    | ExTopOfStackApplied IXIYHL


applyRegisterChange : RegisterChange -> FlagRegisters -> MainWithIndexRegisters -> RegisterChangeApplied
applyRegisterChange change z80_flags main =
    case change of
        ChangeRegisterBC b_value c_value ->
            MainRegsApplied { main | b = b_value, c = c_value }

        ChangeRegisterDE d_value e_value ->
            MainRegsApplied { main | d = d_value, e = e_value }

        ChangeRegisterHL int ->
            MainRegsApplied { main | hl = int }

        ChangeRegisterIX int ->
            MainRegsApplied { main | ix = int }

        ChangeRegisterIY int ->
            MainRegsApplied { main | iy = int }

        ChangeRegisterB int ->
            MainRegsApplied { main | b = int }

        ChangeRegisterD int ->
            MainRegsApplied { main | d = int }

        ChangeRegisterA int ->
            FlagRegsApplied { z80_flags | a = int }

        SingleEnvFlagFunc flagFunc value ->
            FlagRegsApplied (z80_flags |> changeFlags flagFunc value)

        ChangeRegisterE int ->
            MainRegsApplied { main | e = int }

        ChangeRegisterC int ->
            MainRegsApplied { main | c = int }

        ChangeRegisterH int ->
            MainRegsApplied { main | hl = Bitwise.or (Bitwise.and main.hl 0xFF) (shiftLeftBy8 int) }

        ChangeRegisterL int ->
            MainRegsApplied { main | hl = Bitwise.or (Bitwise.and main.hl 0xFF00) int }

        ChangeRegisterIXH int ->
            MainRegsApplied { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF) (int |> shiftLeftBy8) }

        ChangeRegisterIYH int ->
            MainRegsApplied { main | iy = Bitwise.or (Bitwise.and main.iy 0xFF) (int |> shiftLeftBy8) }

        ChangeRegisterIXL int ->
            MainRegsApplied { main | ix = Bitwise.or (Bitwise.and main.ix 0xFF00) int }

        ChangeRegisterIYL int ->
            MainRegsApplied { main | iy = Bitwise.or (Bitwise.and main.iy 0xFF00) int }

        PushedValue int ->
            PushedValueApplied int

        RegChangeNewSP int ->
            NewSPApplied int

        IncrementIndirect int ->
            IncrementIndirectApplied int

        DecrementIndirect int ->
            DecrementIndirectApplied int

        RegisterChangeJump int ->
            JumpApplied int

        SetIndirect addr value ->
            SetIndirectApplied addr value

        ChangeRegisterDEAndHL de hl ->
            MainRegsApplied ({ main | hl = hl } |> set_de_main de)

        RegisterChangeShifter shifter int ->
            RegisterChangeShifterApplied shifter int

        IndirectBitReset bitTest int ->
            IndirectBitResetApplied bitTest int

        IndirectBitSet bitTest int ->
            IndirectBitSetApplied bitTest int

        RegChangeNoOp ->
            RegChangeAppliedNoOp

        RegChangeIm int ->
            RegChangeImApplied int

        ExchangeTopOfStackWith ixiyhl ->
            ExTopOfStackApplied ixiyhl
