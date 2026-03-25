module RegisterChange exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime)
import SingleByteWithEnv exposing (SingleByteEnvChange)
import SingleWith8BitParameter exposing (JumpChange, Single8BitChange)
import TripleByte exposing (TripleByteChange, TripleByteIndexChange)
import TripleWithFlags exposing (TripleWithFlagsChange)
import Utils exposing (BitTest)
import Z80Change exposing (IndexedZ80Change, Z80Change)
import Z80Core exposing (DirectionForLDIR)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagFunc, FlagRegisters)
import Z80Registers exposing (ChangeMainRegister, ChangeSingle, CoreRegister)
import Z80Types exposing (IXIYHL, InterruptMode, MainWithIndexRegisters, Z80ROM)


type Shifter
    = Shifter0
    | Shifter1
    | Shifter2
    | Shifter3
    | Shifter4
    | Shifter5
    | Shifter6
    | Shifter7


type SingleEnvMainChange
    = SingleEnvNewARegister Int CpuTimeCTime
    | SingleEnv8BitMain CoreRegister Int CpuTimeCTime
    | SingleEnvNewHLRegister Int CpuTimeCTime
    | IndirectBitTest BitTest Int
    | SingleEnvMainFlagFunc FlagFunc Int CpuTimeCTime
    | SingleEnvNewHL16BitAdd IXIYHL Int Int


type RegisterFlagChange
    = Pushed16BitValue (MainWithIndexRegisters -> Int)
    | RegChangeNewSP (MainWithIndexRegisters -> Int)
    | IncrementIndirect (MainWithIndexRegisters -> Int)
    | DecrementIndirect (MainWithIndexRegisters -> Int)
    | RegisterChangeJump (MainWithIndexRegisters -> Int)
    | SetIndirect (MainWithIndexRegisters -> ( Int, Int ))
    | RegisterChangeShifter Shifter (MainWithIndexRegisters -> Int)
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
    | FlagNewRValue Int
    | FlagNewIValue Int
    | FlagChangeFunc (FlagRegisters -> FlagRegisters)
    | FlagChangeMain (FlagRegisters -> MainWithIndexRegisters -> MainWithIndexRegisters)
    | ConditionalReturn (FlagRegisters -> Bool)
    | FlagsPushAF
    | PopBC
    | PopDE
    | PopHL
    | PopIX
    | PopIY
    | PopAF
    | Ret
    | Rst Int
    | RegisterZ80Change (MainWithIndexRegisters -> FlagRegisters -> Z80Change)
    | IndexedRegisterZ80Change (MainWithIndexRegisters -> FlagRegisters -> IndexedZ80Change)
    | RegisterSingleByteEnv (Z80Env -> SingleByteEnvChange)
    | RegisterEnvMainChangeWithClockTime (MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange)
    | RegisterEnvMainChange (MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange)


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


type ThreeByteChange
    = ThreeByteFlags TripleWithFlagsChange
    | ThreeBytePlain TripleByteChange
