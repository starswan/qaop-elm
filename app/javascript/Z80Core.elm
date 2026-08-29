module Z80Core exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime, CpuTimePcAnd16BitValue, ShortDelay)
import Interrupts exposing (IFFValue, InterruptRegisters)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Types exposing (MainRegisters, MainWithIndexRegisters)


type alias Z80Core =
    { env : Z80Env
    , main : MainWithIndexRegisters
    , flags : FlagRegisters
    , interrupts : InterruptRegisters
    }


type RepeatPCOffset
    = NoOffset
    | JumpBack



-- SetStackPointer can be used for a pop/return so useful to have its own type


type RareCoreChange
    = CoreOnly Z80Core
    | Z80OutChange Int
    | NewInterrupts InterruptRegisters



-- NoCore can be jumps not taken or calls not made


type CoreChange
    = SetMem8 Int Int
    | NoCore
    | SetMem16 Int Int
    | SetStackPointer Int
    | Push16BitValue Int
    | JumpOnlyPC Int
    | JumpWithOffset Int
    | JumpOffsetWithDelay Int ShortDelay
    | CallWithPCAndDelay Int ShortDelay
    | Looper RepeatPCOffset Z80Core
    | LooperWithDelay RepeatPCOffset ShortDelay Z80Core
    | MainOnly MainWithIndexRegisters
    | FlagsOnly FlagRegisters
    | MainWithOffsetAndDelay Int ShortDelay MainWithIndexRegisters
    | RareChange RareCoreChange
    | PopIntoPC
    | SetMem8Flags Int IntWithFlags
    | ChangeMainAndFlags MainWithIndexRegisters FlagRegisters
    | ChangeMainAndSP MainWithIndexRegisters Int
    | ChangeFlagsAndSP FlagRegisters Int


type DirectionForLDIR
    = Forwards
    | Backwards



--	int af() {return A<<8 | flags();}
--get_af_z80 : Z80 -> Int
--get_af_z80 z80 =
--    z80.flags |> get_af
