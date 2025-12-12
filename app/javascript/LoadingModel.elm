module LoadingModel exposing (..)

import Dict exposing (Dict)
import Http
import MessageHandler exposing (bytesToRom)
import Qaop exposing (Qaop)
import QaopModel exposing (QaopMessage, QaopModel, tapLoad)
import Spectrum
import Time
import Z80Debug exposing (debugLog)


type SpectrumRom
    = RomURL String
    | ROM (Dict Int Int)


type alias LoadingModel =
    { rom : SpectrumRom
    , maybeTime : Maybe Time.Posix
    , tapUrl : String
    }


type InitMessage
    = GotRom (Result Http.Error (Dict Int Int))
    | InitTick Time.Posix


type LoadResult
    = StillLoading LoadingModel
    | NowRunning QaopModel


type alias Flags =
    { rom : String
    , tape : String
    }


loadingInit : Flags -> ( LoadingModel, Cmd InitMessage )
loadingInit data =
    ( LoadingModel (RomURL data.rom) Nothing data.tape, romLoad data.rom )


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
                Ok value ->
                    let
                        newModel =
                            { loadingModel | rom = ROM value }
                    in
                    ( StillLoading newModel, Cmd.none )

                Err _ ->
                    ( StillLoading loadingModel, Cmd.none )

        InitTick posix ->
            case loadingModel.rom of
                RomURL _ ->
                    ( StillLoading loadingModel, Cmd.none )

                ROM z80ROM ->
                    let
                        qaop =
                            Qaop (Spectrum.constructor z80ROM) 0 []

                        qaopModel =
                            QaopModel qaop 0 0 posix False False
                    in
                    ( NowRunning qaopModel, tapLoad loadingModel.tapUrl )


loadingSubs : Int -> Sub InitMessage
loadingSubs tickInterval =
    Time.every (tickInterval |> toFloat) (\posix -> InitTick posix)
