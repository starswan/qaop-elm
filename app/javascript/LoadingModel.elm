module LoadingModel exposing (..)

import Dict exposing (Dict)
import Http
import MessageHandler exposing (bytesToRom)
import Qaop exposing (Qaop)
import QaopModel exposing (QaopMessage, QaopModel, tapLoad)
import Spectrum
import Task
import Time exposing (Posix, millisToPosix)
import Z80Debug exposing (debugLog)


type alias LoadingModel =
    { tapUrl : String
    , currentTime : Posix
    }


type InitMessage
    = GotRom (Result Http.Error (Dict Int Int))


type LoadResult
    = StillLoading LoadingModel
    | NowRunning QaopModel


type alias Flags =
    { rom : String
    , tape : String
    , time : Int
    }


loadingInit : Flags -> ( LoadingModel, Cmd InitMessage )
loadingInit data =
    let
        load =
            romLoad data.rom
    in
    ( LoadingModel data.tape (data.time |> millisToPosix), load )


romLoad : String -> Cmd InitMessage
romLoad url =
    debugLog "loadRom"
        url
        Http.get
        { url = url

        --, expect = Http.Detailed.expectBytes GotRom (array_decoder 16384 unsignedInt8)
        , expect = Http.expectBytesResponse (\result -> GotRom result) bytesToRom
        }


updateLoading : InitMessage -> LoadingModel -> ( LoadResult, Cmd QaopMessage )
updateLoading initMessage loadingModel =
    case initMessage of
        GotRom result ->
            case result of
                Ok z80rom ->
                    let
                        qaop =
                            Qaop (Spectrum.constructor z80rom) 0 []

                        qaopModel =
                            QaopModel qaop 0 0 loadingModel.currentTime False False
                    in
                    ( NowRunning qaopModel, tapLoad loadingModel.tapUrl )

                Err _ ->
                    ( StillLoading loadingModel, Cmd.none )


loadingSubs : Int -> Sub InitMessage
loadingSubs tickInterval =
    Sub.none
