--
-- $Id$
--


module Loader exposing (..)

import Http
import MessageHandler exposing (bytesToRom, bytesToTap)
import Tapfile exposing (Tapfile)
import Time
import Utils exposing (compact)
import Z80Debug exposing (debugLog)
import Z80Rom exposing (Z80ROM)


type LoadAction
    = LoadROM String
    | LoadTAP String


type alias Loader =
    { actions : List LoadAction
    }


type Message
    = Pause
    | GotTAP (Result Http.Error (List Tapfile))
    | GotRom (Result Http.Error Z80ROM)
    | Tick Time.Posix
    | Autoload
    | CharacterKeyDown Char
    | CharacterUnKey Char
    | ControlKeyDown String
    | ControlUnKey String
    | KeyRepeat
    | CharacterKeyUp Char
    | ControlKeyUp String


trimActionList : Maybe (List LoadAction) -> List LoadAction
trimActionList tail =
    case tail of
        Just a ->
            a

        Nothing ->
            []


paramHandler : ( String, String ) -> Maybe LoadAction
paramHandler ( first, second ) =
    if first == "rom" then
        Just (LoadROM second)

    else if first == "tape" then
        Just (LoadTAP second)

    else
        Nothing


new : List ( String, String ) -> Loader
new params =
    let
        paramlist =
            List.map paramHandler params

        compacted_params =
            compact paramlist
    in
    Loader compacted_params


actionToCmd : LoadAction -> Cmd Message
actionToCmd action =
    case action of
        LoadTAP url ->
            -- Not sure if this is helping - we want to pick out 16384 from the metadata so we know how many bytes to consume
            -- as TAP files will not always be the same size...
            --Http.request { method = "GET",
            --               headers = [],
            --               body = emptyBody,
            --               timeout = Nothing,
            --               tracker = Nothing,
            --               url = String.concat ["http://localhost:3000/", fileName],
            --               expect = Http.expectBytesResponse GotTAP convertResponse
            --          }
            debugLog "loadTap"
                url
                Http.get
                { url = url
                , expect = Http.expectBytesResponse GotTAP bytesToTap
                }

        LoadROM url ->
            --loadRom: String -> Cmd Message
            --loadRom fileName =
            --    Http.get { url = String.concat ["http://localhost:3000/", fileName],
            --               expect = Http.expectBytes GotRom (list_decoder 16384 unsignedInt8)
            --              }
            debugLog "loadRom"
                url
                Http.get
                { url = url

                --, expect = Http.Detailed.expectBytes GotRom (array_decoder 16384 unsignedInt8)
                , expect = Http.expectBytesResponse GotRom bytesToRom
                }


run : Loader -> ( Loader, Maybe LoadAction )
run loader =
    let
        nextAction =
            List.head loader.actions

        qaop_1 =
            { loader | actions = List.tail loader.actions |> trimActionList }
    in
    ( qaop_1, nextAction )



--case nextAction of
--    Just action ->
--        ( qaop_1, actionToCmd action )
--
--    Nothing ->
--        ( qaop_1, Cmd.none )
