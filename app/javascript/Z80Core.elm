module Z80Core exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimePcAnd16BitValue, addCpuTimeTime)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (InterruptMode(..), InterruptRegisters, MainRegisters, MainWithIndexRegisters)


type alias Z80Core =
    { env : Z80Env
    , pc : Int
    , main : MainWithIndexRegisters
    , flags : FlagRegisters
    , interrupts : InterruptRegisters
    }


type CoreChange
    = CoreOnly Z80Core
    | CoreWithTime ShortDelay Z80Core


type DirectionForLDIR
    = Forwards
    | Backwards



--	private int imm16()
--	{
--		int v = env.mem16(PC);
--		PC = (char)(PC+2);
--		time += 6;
--		return v;
--	}


imm16 : Z80ROM -> CpuTimeCTime -> Z80Core -> CpuTimePcAnd16BitValue
imm16 rom48k clockTime z80 =
    let
        v =
            z80.env |> mem16 z80.pc rom48k clockTime

        pc =
            Bitwise.and (z80.pc + 2) 0xFFFF

        env =
            v.time |> addCpuTimeTime 6
    in
    CpuTimePcAnd16BitValue env pc v.value16


set_iff : Int -> Z80Core -> InterruptRegisters
set_iff value z80 =
    let
        --logging this can be noisy
        --interrupts =
        --    debugLog "set_iff" value z80.interrupts
        interrupts =
            z80.interrupts
    in
    { interrupts | iff = value }



--	int af() {return A<<8 | flags();}
