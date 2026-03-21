--
-- $Id$
--


module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, div)
import LoadingModel exposing (Flags, InitMessage(..), LoadResult(..), LoadingModel, SpectrumRom(..), loadingInit, loadingSubs, updateLoading)
import QaopModel exposing (QaopMessage, QaopModel, qaopSubs, updateQaop, viewQaop)



-- meant to be run every 20 msec(50Hz)
-- arthur timings:
--  6th Nov 2024 Chromium debug 59.9ms (16.76 Hz)  64 sec live 29.9ms (32.2 Hz)  60 sec
--  4th Oct 2024 Chromium debug 61.9ms (16.1 Hz) 174 sec live 31.7ms (31.5 Hz)
--  4th Oct 2024 Firefox  debug 81.0ms (12.3 Hz) 105 sec live 56.8ms (17.6 Hz)
-- 20th Jul 2024 Chromium                                live 37.9ms (28.5 Hz)
-- 12th Jul 2024 Firefox  debug        (12.4 Hz)         live        (17.9 Hz)
--
-- 23rd Jun 2024 Chromium debug 70.5ms (14.2 Hz) 106 sec live 36.3ms (27.6 Hz) 163 sec
-- 23rd Jun 2024 Firefox                                 live 61.5ms (16.2 Hz) 160 sec
--
-- 29th Jan 2024 Chromium debug 69.9ms (14.3 Hz) 365 sec live 37.8ms (26.6 Hz)
-- 29th Jan 2024 Firefox  debug 95.1ms (10.5 Hz)         live 59.3ms (16.8 Hz) 900 sec
-- Run at 25 (40Hz) - i7 laptop can do 20Hz in firefox dev mode


c_TICKTIME =
    20



-- I'm currently unsure whether scaling the display results in a significant slowdown or not
-- what it does show is that changing the screen makes everything slower, which probably means in practice
-- that the display code will need some optimisation


type ModelState
    = Loading LoadingModel
    | Running QaopModel


type alias Model =
    { state : ModelState
    , -- This doesn't look like a variable, but think it might be useful to adjust it at runtime
      tickInterval : Int
    }


type Message
    = LoadingMessage InitMessage
    | RunningMessage QaopMessage


init : Flags -> ( Model, Cmd Message )
init data =
    let
        ( m, c ) =
            loadingInit data
    in
    ( Model (m |> Loading) c_TICKTIME, c |> Cmd.map LoadingMessage )


view : Model -> Html Message
view model =
    case model.state of
        Loading _ ->
            div [] []

        Running qaopModel ->
            viewQaop qaopModel model.tickInterval |> Html.map RunningMessage


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        LoadingMessage initMessage ->
            case model.state of
                Loading loadingModel ->
                    let
                        ( state, qaopCmd ) =
                            updateLoading initMessage loadingModel

                        cmd =
                            qaopCmd |> Cmd.map RunningMessage
                    in
                    case state of
                        StillLoading newloadingModel ->
                            ( { model | state = Loading newloadingModel }, cmd )

                        NowRunning qaopModel ->
                            ( { model | state = Running qaopModel }, cmd )

                Running _ ->
                    ( model, Cmd.none )

        RunningMessage qaopMessage ->
            case model.state of
                Loading _ ->
                    ( model, Cmd.none )

                Running qaopModel ->
                    let
                        ( x, y ) =
                            updateQaop qaopMessage qaopModel
                    in
                    ( { model | state = Running x }, y |> Cmd.map RunningMessage )


subscriptions : Model -> Sub Message
subscriptions model =
    case model.state of
        Loading _ ->
            loadingSubs model.tickInterval |> Sub.map LoadingMessage

        Running qaopModel ->
            qaopSubs qaopModel model.tickInterval |> Sub.map RunningMessage


main : Program Flags Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
