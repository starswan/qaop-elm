--
-- $Id$
--


module Loader exposing (..)

import Http
import MessageHandler exposing (bytesToRom, bytesToTap)
import Tapfile exposing (Tapfile)
import Time
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
