module Group0xF0 exposing (..)

import Dict exposing (Dict)
import Group0x70 exposing (miniDict70)
import Group0xC0 exposing (miniDictC0)
import Z80Core exposing (Z80Core)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..))


xYDict : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
xYDict =
    miniDict70
        |> Dict.union miniDictC0
