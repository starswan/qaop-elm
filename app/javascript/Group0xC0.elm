module Group0xC0 exposing (..)

import Dict exposing (Dict)
import GroupCB exposing (group_xy_cb)
import Z80Core exposing (Z80Core)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..))


miniDictC0 : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
miniDictC0 =
    Dict.fromList
        [ ( 0xCB, execute_0xCB )
        ]


execute_0xCB : IXIY -> Z80ROM -> Z80Core -> Z80Delta
execute_0xCB ixiyhl rom48k z80 =
    case ixiyhl of
        IXIY_IX ->
            z80 |> group_xy_cb IXIY_IX rom48k

        IXIY_IY ->
            z80 |> group_xy_cb IXIY_IY rom48k
