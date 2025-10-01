module Interrupts exposing (..)


type InterruptMode
    = IM0
    | IM1
    | IM2


type IFFValue
    = IFF_0
    | IFF_3


type alias InterruptRegisters =
    { --mp:  Int, -- /* MEMPTR, the hidden register emulated according to memptr_eng.txt */
      iM : InterruptMode
    , halted : Bool
    , ir : Int
    , r : Int
    }
