module RegisterChange exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)
import Interrupts exposing (InterruptMode)
import JumpChange exposing (JumpChange)
import SingleEnvWithMain exposing (SingleEnvMainChange)
import SingleWith8BitParameter exposing (Single8BitChange)
import Z80Change exposing (IndexedZ80Change, Z80Change)
import Z80Core exposing (DirectionForLDIR)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters)
import Z80Registers exposing (ChangeMainRegister, ChangeSingle, CoreRegister)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL, MainWithIndexRegisters)


type Shifter
    = Shifter0
    | Shifter1
    | Shifter2
    | Shifter3
    | Shifter4
    | Shifter5
    | Shifter6
    | Shifter7


type Pop16
    = PopBC
    | PopDE
    | PopHL
    | PopIX
    | PopIY
    | PopAF


type RegisterFlagChange
    = Pushed16BitValue (MainWithIndexRegisters -> Int)
    | RegChangeNewSP (MainWithIndexRegisters -> Int)
    | IncrementIndirect (MainWithIndexRegisters -> Int)
    | DecrementIndirect (MainWithIndexRegisters -> Int)
    | RegisterChangeJump (MainWithIndexRegisters -> Int)
    | SetIndirect (MainWithIndexRegisters -> ( Int, Int ))
    | RegChangeNoOp
    | SingleEnvFlagFunc (Int -> FlagRegisters -> FlagRegisters) (MainWithIndexRegisters -> Int)
    | ExchangeTopOfStackWith IXIYHL
    | RegisterChangeA (MainWithIndexRegisters -> Int)
    | FlagNewRValue Int
    | FlagNewIValue Int
    | FlagChangeFunc (FlagRegisters -> FlagRegisters)
    | FlagChangeMain (FlagRegisters -> MainWithIndexRegisters -> MainWithIndexRegisters)
    | ConditionalReturn (FlagRegisters -> Bool)
    | FlagsPushAF
    | Pop16Bit Pop16
    | Ret
    | Rst Int
    | RegisterZ80Change (MainWithIndexRegisters -> FlagRegisters -> Z80Change)
    | IndexedRegisterZ80Change (MainWithIndexRegisters -> FlagRegisters -> IndexedZ80Change)
    | RegisterEnvMainChangeWithClockTime (MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange)
    | RegisterEnvMainChange (MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange)
    | LoadAIndirect (MainWithIndexRegisters -> Int)
    | LoadRegisterIndirect ChangeMainRegister (MainWithIndexRegisters -> Int)
    | FlagFuncIndirect (Int -> FlagRegisters -> FlagRegisters) (MainWithIndexRegisters -> Int)
    | SetMemIndirectFromA (MainWithIndexRegisters -> Int)
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


type TwoByteChange
    = TwoByte8Bit Single8BitChange
    | TwoByteJump JumpChange
