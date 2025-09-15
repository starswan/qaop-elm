--
-- $Id$
--


module Main exposing (..)

import Browser
import Delay
import Dict
import Html exposing (Attribute, Html, button, div, h2, span, text)
import Html.Attributes exposing (disabled, id, style, tabindex)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Decode exposing (Decoder)
import Keyboard exposing (ctrlKeyDownEvent, ctrlKeyUpEvent, keyDownEvent, keyUpEvent)
import Loader exposing (LoadAction(..), Message(..), actionToCmd)
import Qaop exposing (Qaop, pause)
import ScreenStorage exposing (ScreenLine, Z80Screen)
import Spectrum exposing (Spectrum, frames, new_tape)
import SpectrumColour exposing (borderColour)
import Svg exposing (Svg, g, line, rect, svg)
import Svg.Attributes exposing (fill, height, rx, stroke, viewBox, width, x1, x2, y1, y2)
import Svg.Lazy
import Tapfile exposing (Tapfile)
import Time exposing (posixToMillis)
import Utils exposing (speed_in_hz, time_display)
import Vector24 exposing (Vector24)
import Vector8 exposing (Vector8)
import Z80Rom exposing (Z80ROM)
import Z80Screen exposing (ScreenColourRun, mapScreenLine)



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


c_SCALEFACTOR =
    2


type AutoKey
    = AutoChar Char
    | AutoControl String


type Model
    = Initial (Maybe Time.Posix) (Maybe Z80ROM) (Maybe (List Tapfile)) String
    | Running QaopModel


type alias QaopModel =
    { qaop : Qaop
    , -- This doesn't look like a variable, but think it might be useful to adjust it at runtime
      tickInterval : Int
    , count : Int
    , elapsed_millis : Int
    , time : Time.Posix
    , loadPressed : Bool
    }


type alias Flags =
    { rom : String
    , tape : String
    }


init : Flags -> ( Model, Cmd Message )
init data =
    ( Initial Nothing Nothing Nothing data.tape, actionToCmd (LoadROM data.rom) )


mapLineToSvg : Int -> ( Int, ScreenColourRun ) -> Svg Message
mapLineToSvg y_index ( start, linedata ) =
    line
        [ x1 (48 + start |> String.fromInt)
        , y1 (40 + y_index |> String.fromInt)
        , x2 ((48 + start + linedata.length) |> String.fromInt)
        , y2 (40 + y_index |> String.fromInt)
        , stroke (linedata.colour |> .colour)
        ]
        []


backgroundNode : Z80Screen -> Svg Message
backgroundNode screen =
    let
        -- border colour is never bright
        border_colour =
            borderColour screen.border
    in
    rect [ height "100%", width "100%", fill border_colour, rx "15" ] []


mapScreenLineToSvg : Bool -> Vector24.Index -> ScreenLine -> Svg Message
mapScreenLineToSvg flash index24 screenLine =
    let
        scrFolded : Vector8 (List ( Int, ScreenColourRun ))
        scrFolded =
            screenLine
                |> mapScreenLine flash

        folded2 : Vector8 (List (Svg Message))
        folded2 =
            scrFolded
                |> Vector8.indexedMap
                    (\index8 vec ->
                        let
                            y_index =
                                ((index24 |> Vector24.indexToInt) * 8) + (index8 |> Vector8.indexToInt)
                        in
                        vec |> List.map (mapLineToSvg y_index)
                    )

        folded4 : List (Svg Message)
        folded4 =
            folded2 |> Vector8.map (\row -> g [] row) |> Vector8.toList

        folded5 : Svg Message
        folded5 =
            folded4 |> g []
    in
    folded5


screenDataNodeList : Bool -> Vector24 ScreenLine -> Vector24 (Svg Message)
screenDataNodeList flash screenLines =
    screenLines |> Vector24.indexedMap (\index line -> line |> Svg.Lazy.lazy3 mapScreenLineToSvg flash index)


svgNode : Z80Screen -> Html Message
svgNode screen =
    let
        nodelist =
            screenDataNodeList screen.flash

        screenLines =
            screen.lines |> nodelist |> Vector24.toList
    in
    svg
        [ height (272 * c_SCALEFACTOR |> String.fromInt)
        , width (352 * c_SCALEFACTOR |> String.fromInt)
        , viewBox "0 0 352 272"
        ]
        --<rect width="100%" height="100%" fill="green" />
        [ Svg.Lazy.lazy backgroundNode screen

        --, g [] (screenDataNodes screen)
        , g [] screenLines
        ]


view : Model -> Html Message
view model_state =
    case model_state of
        Running model ->
            let
                screen =
                    model.qaop.spectrum.rom48k.z80ram.screen

                load_disabled =
                    case model.qaop.spectrum.tape of
                        Just tape ->
                            model.loadPressed || model.count < 100 || tape.tapePos.tapfileNumber == (tape.tapfiles |> Dict.size)

                        Nothing ->
                            True

                speed =
                    speed_in_hz model.elapsed_millis model.count

                time_disp =
                    time_display model.elapsed_millis model.count
            in
            -- The inline style is being used for example purposes in order to keep this example simple and
            -- avoid loading additional resources. Use a proper stylesheet when building your own app.
            div []
                [ h2 [] [ text ("Refresh Interval " ++ (model.tickInterval |> String.fromInt) ++ "ms ") ]
                , div [ style "display" "flex", style "justify-content" "center" ]
                    [ div [] [ span [ id "cyclecount" ] [ text (String.fromInt model.count) ], text " in ", text time_disp, span [ id "hz" ] [ text speed ], text " Hz" ]
                    , button [ onClick Pause ]
                        [ text
                            (if model.qaop.spectrum.paused then
                                "Unpause"

                             else
                                "Pause"
                            )
                        ]
                    , button [ onClick Autoload, disabled load_disabled ]
                        [ text "Load"
                        ]
                    ]
                , div
                    [ tabindex 0
                    , id "spectrum"
                    , preventDefaultOn "keydown" (Decode.map alwaysPreventDefault keyDownDecoder)
                    , preventDefaultOn "keyup" (Decode.map alwaysPreventDefault keyUpDecoder)
                    ]
                    [ Svg.Lazy.lazy svgNode screen
                    ]

                --,svg [style "height" "192px", style "width" "256px"] (List.indexedMap lineListToSvg lines |> List.concat)
                ]

        _ ->
            div [] []


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


update : Message -> Model -> ( Model, Cmd Message )
update message model_state =
    case model_state of
        Initial time mayberom maybetap tapname ->
            case message of
                Tick posix ->
                    let
                        romtap =
                            Maybe.map2 (\justrom justtap -> ( justrom, justtap )) mayberom maybetap

                        new_model =
                            case romtap of
                                Just ( justrom, justtap ) ->
                                    let
                                        qaop =
                                            Qaop.new

                                        speccy =
                                            qaop.spectrum |> new_tape justtap

                                        qaop_2 =
                                            { qaop | spectrum = { speccy | rom48k = justrom } } |> unpause

                                        qaop_model =
                                            QaopModel qaop_2 c_TICKTIME 0 0 posix False
                                    in
                                    Running qaop_model

                                Nothing ->
                                    model_state
                    in
                    ( new_model, Cmd.none )

                GotRom result ->
                    case result of
                        Ok value ->
                            ( Initial time (Just value) maybetap tapname, actionToCmd (LoadTAP tapname) )

                        Err _ ->
                            ( model_state, Cmd.none )

                GotTAP result ->
                    case result of
                        Ok value ->
                            ( Initial time mayberom (Just value) tapname, Cmd.none )

                        Err _ ->
                            ( model_state, Cmd.none )

                _ ->
                    ( model_state, Cmd.none )

        Running model ->
            let
                ( qaopModel, qaopCmd ) =
                    case message of
                        Tick posix ->
                            let
                                state =
                                    if model.qaop.spectrum.paused then
                                        { qaop = model.qaop, cmd = Cmd.none, count = model.count, elapsed = 0 }

                                    else
                                        let
                                            elapsed =
                                                posixToMillis posix - posixToMillis model.time

                                            ( q, cmd ) =
                                                ( model.qaop |> qaop_frames, Cmd.none )
                                        in
                                        { qaop = q, cmd = cmd, count = model.count + 1, elapsed = elapsed }
                            in
                            ( { model | count = state.count, elapsed_millis = model.elapsed_millis + state.elapsed, time = posix, qaop = state.qaop }, state.cmd )

                        Pause ->
                            ( { model | qaop = model.qaop |> pause (not model.qaop.spectrum.paused) }, Cmd.none )

                        CharacterKeyDown char ->
                            let
                                qaop =
                                    model.qaop
                            in
                            ( { model | qaop = { qaop | keys = qaop.keys |> keyDownEvent char } }, Cmd.none )

                        ControlKeyDown str ->
                            let
                                qaop =
                                    model.qaop
                            in
                            ( { model | qaop = { qaop | keys = qaop.keys |> ctrlKeyDownEvent str } }, Cmd.none )

                        CharacterUnKey char ->
                            let
                                qaop =
                                    model.qaop
                            in
                            ( { model | qaop = { qaop | keys = qaop.keys |> keyUpEvent char } }, Cmd.none )

                        ControlUnKey str ->
                            let
                                qaop =
                                    model.qaop
                            in
                            ( { model | qaop = { qaop | keys = qaop.keys |> ctrlKeyUpEvent str } }, Cmd.none )

                        KeyRepeat ->
                            -- do nothing on repeating keys
                            ( model, Cmd.none )

                        -- This tiny sleep makes the keyboard work in capybara
                        CharacterKeyUp char ->
                            ( model, Delay.after 2 (CharacterUnKey char) )

                        ControlKeyUp str ->
                            ( model, Delay.after 2 (ControlUnKey str) )

                        Autoload ->
                            ( { model | loadPressed = True }, Cmd.batch loadingCommands )

                        _ ->
                            ( model, Cmd.none )
            in
            ( Running qaopModel, qaopCmd )


loadQuoteQuoteEnter =
    [ AutoChar 'j', AutoChar '"', AutoChar '"', AutoControl "Enter" ]


c_LOADING_KEY_DELAY =
    800


loadingCommands : List (Cmd Message)
loadingCommands =
    loadQuoteQuoteEnter
        |> List.indexedMap
            (\index item ->
                let
                    ( down, up ) =
                        case item of
                            AutoChar char ->
                                ( CharacterKeyDown char, CharacterKeyUp char )

                            AutoControl string ->
                                ( ControlKeyDown string, ControlKeyUp string )
                in
                -- TODO: loading delay is based on time not cycles. Once keyboard
                -- scanner is always run at 50Hz, this value could be reduced
                [ Delay.after (c_LOADING_KEY_DELAY * index) down
                , Delay.after (c_LOADING_KEY_DELAY * index + c_LOADING_KEY_DELAY // 2) up
                ]
            )
        |> List.concat


subscriptions : Model -> Sub Message
subscriptions model_state =
    case model_state of
        Initial _ _ _ _ ->
            Time.every (c_TICKTIME |> toFloat) Tick

        --WithTime _ _ ->
        --    Sub.none
        Running model ->
            --if model.qaop.spectrum.paused || not (List.isEmpty model.qaop.loader.actions) then
            if model.qaop.spectrum.paused then
                Sub.none

            else
                Time.every (model.tickInterval |> toFloat) Tick


keyDownDecoder : Decode.Decoder Message
keyDownDecoder =
    Decode.map2 toKey (Decode.field "key" Decode.string) (Decode.field "repeat" Decode.bool)


keyUpDecoder : Decode.Decoder Message
keyUpDecoder =
    Decode.map toUnKey (Decode.field "key" Decode.string)


toKey : String -> Bool -> Message
toKey keyValue repeat =
    if repeat then
        KeyRepeat

    else
        case String.uncons keyValue of
            Just ( char, "" ) ->
                CharacterKeyDown char

            _ ->
                ControlKeyDown keyValue


toUnKey : String -> Message
toUnKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKeyUp char

        _ ->
            ControlKeyUp keyValue



--	public void run() {
--		Loader l;
--		for(;;) try {
--			synchronized(queue) {
--				if(queue.isEmpty()) {
--					state &= ~2;
--					spectrum.pause(010);
--					queue.wait();
--					continue;
--				}
--				state |= 2;
--				l = (Loader)queue.remove(0);
--			}
--			l.exec();
--		} catch(InterruptedException x) {
--			break;
--		} catch(Exception x) {
--			x.printStackTrace();
--		}
--	}


unpause : Qaop -> Qaop
unpause qaop =
    { qaop | spectrum = qaop.spectrum |> Spectrum.pause 0x08 }


qaop_frames : Qaop -> Qaop
qaop_frames qaop =
    { qaop | spectrum = qaop.spectrum |> frames qaop.keys }


main : Program Flags Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
