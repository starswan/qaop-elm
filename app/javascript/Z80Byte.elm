module Z80Byte exposing (..)

import Hexcode exposing (Hexcode)


type alias Z80Byte =
    { value : Int
    , code : Hexcode
    }


fromInt : Int -> Z80Byte
fromInt int =
    { value = int, code = int |> Hexcode.fromInt }
