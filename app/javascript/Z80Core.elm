module Z80Core exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime, CpuTimePcAnd16BitValue, ShortDelay)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (InterruptMode(..), InterruptRegisters, MainRegisters, MainWithIndexRegisters)


type alias Z80Core =
    { env : Z80Env
    , main : MainWithIndexRegisters
    , flags : FlagRegisters
    , interrupts : InterruptRegisters
    }


type RepeatPCOffset
    = NoOffset
    | JumpBack


type CoreChange
    = CoreOnly Z80Core
    | NoCore
    | CoreWithTime ShortDelay Z80Core
    | CoreWithPC Int Z80Core
    | JumpOnlyPC Int
    | JumpWithOffset Int
    | JumpOffsetWithDelay Int ShortDelay
    | CoreWithOffsetAndDelay Int ShortDelay Z80Core
    | CallWithPCAndDelay Int ShortDelay
    | CallWithPC Int
    | Looper RepeatPCOffset Z80Core
    | LooperWithDelay RepeatPCOffset ShortDelay Z80Core
    | MainOnly MainWithIndexRegisters


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
--imm16 : Z80ROM -> CpuTimeCTime -> Z80Core -> CpuTimePcAnd16BitValue
--imm16 rom48k clockTime z80 =
--    let
--        v =
--            z80.env |> mem16 z80.pc rom48k clockTime
--
--        pc =
--            Bitwise.and (z80.pc + 2) 0xFFFF
--
--        env =
--            v.time |> addCpuTimeTime 6
--    in
--    CpuTimePcAnd16BitValue env pc v.value16


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
--get_af_z80 : Z80 -> Int
--get_af_z80 z80 =
--    z80.flags |> get_af
--set408bitHL : Int -> Int -> CpuTimeCTime -> ( MainWithIndexRegisters, FlagRegisters, Z80Env ) -> ( MainWithIndexRegisters, FlagRegisters, Z80Env )
--set408bitHL c value clockTime ( z80_main, z80_flags, z80_env ) =
--    case Bitwise.and c 0x07 of
--        0 ->
--            ( { z80_main | b = value }, z80_flags, z80_env )
--
--        1 ->
--            ( { z80_main | c = value }, z80_flags, z80_env )
--
--        2 ->
--            ( { z80_main | d = value }, z80_flags, z80_env )
--
--        3 ->
--            ( { z80_main | e = value }, z80_flags, z80_env )
--
--        4 ->
--            ( { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 value) }, z80_flags, z80_env )
--
--        5 ->
--            ( { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) value }, z80_flags, z80_env )
--
--        -- This is only used by ED40-ED78, and ED70 is supposed to throw away the result
--        --6 ->
--        --    ( z80_main, z80_flags, z80_env |> setMemIgnoringTime z80_main.hl value clockTime )
--        7 ->
--            ( z80_main, { z80_flags | a = value }, z80_env )
--
--        _ ->
--            ( z80_main, z80_flags, z80_env )
