module RegisterChange exposing (..)

import Utils exposing (BitTest)
import Z80Byte exposing (Z80Byte)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (MainWithIndexRegisters, set_de_main)
import Z80Word exposing (Z80Word)


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
    = ChangeRegisterBC Z80Byte Z80Byte
    | ChangeRegisterB Z80Byte
    | ChangeRegisterDE Z80Byte Z80Byte
    | ChangeRegisterE Z80Byte
    | ChangeRegisterHL Z80Word
    | ChangeRegisterIX Z80Word
    | ChangeRegisterIY Z80Word
    | ChangeRegisterD Z80Byte
    | ChangeRegisterA Z80Byte
    | ChangeRegisterC Z80Byte
    | ChangeRegisterH Z80Byte
    | ChangeRegisterL Z80Byte
    | PushedValue Z80Word
    | RegChangeNewSP Z80Word
    | IncrementIndirect Z80Word
    | DecrementIndirect Z80Word
    | RegisterChangeJump Z80Word
    | SetIndirect Z80Word Z80Byte
    | ChangeRegisterDEAndHL Z80Word Z80Word
    | RegisterChangeShifter Shifter Z80Word
    | IndirectBitReset BitTest Z80Word
    | IndirectBitSet BitTest Z80Word


type RegisterChangeApplied
    = MainRegsApplied MainWithIndexRegisters
    | FlagRegsApplied FlagRegisters
    | PushedValueApplied Z80Word
    | NewSPApplied Z80Word
    | IncrementIndirectApplied Z80Word
    | DecrementIndirectApplied Z80Word
    | JumpApplied Z80Word
    | SetIndirectApplied Z80Word Z80Byte
    | RegisterChangeShifterApplied Shifter Z80Word
    | IndirectBitResetApplied BitTest Z80Word
    | IndirectBitSetApplied BitTest Z80Word


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

        ChangeRegisterE int ->
            MainRegsApplied { main | e = int }

        ChangeRegisterC int ->
            MainRegsApplied { main | c = int }

        ChangeRegisterH int ->
            --MainRegsApplied { main | hl = Bitwise.or (Bitwise.and main.hl 0xFF) (shiftLeftBy8 int) }
            let
                xl =
                    main.hl
            in
            MainRegsApplied { main | hl = { xl | high = int } }

        ChangeRegisterL int ->
            --MainRegsApplied { main | hl = Bitwise.or (Bitwise.and main.hl 0xFF00) int }
            let
                xl =
                    main.hl
            in
            MainRegsApplied { main | hl = { xl | low = int } }

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
