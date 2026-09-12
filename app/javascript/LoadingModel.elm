module LoadingModel exposing (..)

import Array exposing (Array)
import Http
import MessageHandler exposing (bytesToRom)
import Qaop exposing (Qaop)
import QaopModel exposing (QaopMessage, QaopModel, tapLoad)
import Spectrum
import Time exposing (Posix, millisToPosix)
import Z80Debug exposing (debugLog)
import Z80Rom exposing (Z80ROM)


type alias LoadingModel =
    { tapUrl : String
    , currentTime : Posix
    }


type InitMessage
    = GotRom (Result Http.Error Z80ROM)


type LoadResult
    = StillLoading LoadingModel
    | NowRunning QaopModel


type alias Flags =
    { rom : String
    , tape : String
    , timeInMillis : Int
    }


loadingInit : Flags -> ( LoadingModel, Cmd InitMessage )
loadingInit data =
    let
        load =
            romLoad data.rom
    in
    ( LoadingModel data.tape (data.timeInMillis |> millisToPosix), load )


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
                        qaop : Qaop
                        qaop =
                            Qaop (Spectrum.constructor z80rom) 0 [] True

                        qaopModel =
                            QaopModel qaop 0 0 loadingModel.currentTime False False
                    in
                    ( NowRunning qaopModel, tapLoad loadingModel.tapUrl )

                --( NowRunning qaopModel, Cmd.none )
                Err _ ->
                    ( StillLoading loadingModel, Cmd.none )



--loadingSubs : Int -> Sub InitMessage
--loadingSubs tickInterval =
--    Sub.none
