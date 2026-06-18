module LoadingModel exposing (..)

import Dict exposing (Dict)
import Http
import MessageHandler exposing (bytesToRom)
import Qaop exposing (Qaop)
import QaopModel exposing (QaopMessage, QaopModel, tapLoad)
import Spectrum
import Task
import Time
import Z80Debug exposing (debugLog)


type alias LoadingModel =
    { tapUrl : String
    , state : LoadingState
    }


type LoadingState
    = Initial
    | OnlyTime Time.Posix
    | ROM (Dict Int Int)


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
    let
        load =
            romLoad data.rom

        tick =
            Task.perform InitTick Time.now
    in
    ( LoadingModel data.tape Initial, Cmd.batch [ load, tick ] )


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
                    case loadingModel.state of
                        Initial ->
                            let
                                newModel =
                                    { loadingModel | state = ROM z80rom }
                            in
                            ( StillLoading newModel, Cmd.none )

                        OnlyTime posix ->
                            let
                                qaop =
                                    Qaop (Spectrum.constructor z80rom) 0 []

                                qaopModel =
                                    QaopModel qaop 0 0 posix False False
                            in
                            ( NowRunning qaopModel, tapLoad loadingModel.tapUrl )

                        ROM z80ROM ->
                            ( StillLoading loadingModel, Cmd.none )

                Err _ ->
                    ( StillLoading loadingModel, Cmd.none )

        InitTick posix ->
            case loadingModel.state of
                Initial ->
                    ( StillLoading { loadingModel | state = OnlyTime posix }, Cmd.none )

                OnlyTime _ ->
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
    Sub.none
