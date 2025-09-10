module Z80Types exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, CpuTimePcAnd16BitValue, CpuTimePcAndValue)
import Utils exposing (shiftLeftBy8, shiftRightBy8)
import Z80Env exposing (Z80Env, Z80EnvWithPC)
import Z80Flags exposing (FlagRegisters)


type alias MainRegisters =
    { b : Int
    , c : Int
    , d : Int
    , e : Int
    , hl : Int
    }


type alias MainWithIndexRegisters =
    { b : Int
    , c : Int
    , d : Int
    , e : Int
    , hl : Int
    , ix : Int
    , iy : Int
    , ir : Int
    }


type InterruptMode
    = IM0
    | IM1
    | IM2


type alias InterruptRegisters =
    { --mp:  Int, -- /* MEMPTR, the hidden register emulated according to memptr_eng.txt */
      iM : InterruptMode
    , halted : Bool
    , iff : Int
    }


type alias EnvWithPCAndValue =
    { env : Z80Env
    , pc : Int
    , value : Int
    }


type alias IntWithFlagsTimeAndPC =
    { value : Int
    , flags : FlagRegisters
    , time : CpuTimeCTime
    , pc : Int
    }


type IXIYHL
    = IX
    | IY
    | HL


get_xy : IXIYHL -> MainWithIndexRegisters -> Int
get_xy ixiyhl z80_main =
    case ixiyhl of
        IX ->
            z80_main.ix

        IY ->
            z80_main.iy

        HL ->
            z80_main.hl


set_xy : Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_xy value ixiyhl z80 =
    case ixiyhl of
        IX ->
            { z80 | ix = value }

        IY ->
            { z80 | iy = value }

        HL ->
            { z80 | hl = value }


get_bc : MainWithIndexRegisters -> Int
get_bc z80_main =
    z80_main.b |> shiftLeftBy8 |> Bitwise.or z80_main.c


get_de : MainWithIndexRegisters -> Int
get_de z80 =
    z80.d |> shiftLeftBy8 |> Bitwise.or z80.e


set_bc_main : Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_bc_main v z80_main =
    { z80_main | b = v |> shiftRightBy8, c = Bitwise.and v 0xFF }


set_de_main : Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_de_main v z80_main =
    { z80_main | d = shiftRightBy8 v, e = Bitwise.and v 0xFF }
