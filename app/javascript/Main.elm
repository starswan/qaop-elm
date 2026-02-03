--
-- $Id$
--


module Main exposing (..)

import Bitwise exposing (complement)
import Browser
import Delay
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, h2, span, text)
import Html.Attributes exposing (disabled, id, style, tabindex)
import Html.Events exposing (onClick, preventDefaultOn)
import Http exposing (Metadata)
import Json.Decode as Decode exposing (Decoder)
import Keyboard exposing (ctrlKeyDownEvent, ctrlKeyUpEvent, keyDownEvent, keyUpEvent)
import MessageHandler exposing (bytesToRom, bytesToTap)
import Qaop exposing (Qaop, pause)
import ScreenStorage exposing (ScreenLine, Z80Screen)
import Spectrum exposing (frames, new_tape)
import SpectrumColour exposing (borderColour)
import Svg exposing (Svg, g, line, rect, svg)
import Svg.Attributes exposing (fill, height, rx, stroke, viewBox, width, x1, x2, y1, y2)
import Svg.Lazy
import Tapfile exposing (Tapfile)
import Time exposing (posixToMillis)
import Utils exposing (speed_in_hz, time_display)
import Vector24 exposing (Vector24)
import Vector32
import Vector8 exposing (Vector8)
import Z80Debug exposing (debugLog)
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


type alias QaopModel =
    { qaop : Qaop
    , count : Int
    , elapsed_millis : Int
    , time : Time.Posix
    , loadPressed : Bool
    , globalFlash : Bool
    }


type SpectrumRom
    = RomURL String
    | ROM (Dict Int Int)


type alias LoadingModel =
    { rom : SpectrumRom
    , maybeTime : Maybe Time.Posix
    , tapUrl : String
    }


type ModelState
    = Loading LoadingModel
    | Running QaopModel


type alias Model =
    { state : ModelState
    , -- This doesn't look like a variable, but think it might be useful to adjust it at runtime
      tickInterval : Int
    }


type QaopMessage
    = GotTAP (Result Http.Error (List Tapfile))
    | Tick Time.Posix
    | FlipFlash Time.Posix
    | Pause
    | Autoload
    | CharacterKeyDown Char
    | CharacterUnKey Char
    | ControlKeyDown String
    | ControlUnKey String
    | KeyRepeat
    | CharacterKeyUp Char
    | ControlKeyUp String


type InitMessage
    = GotRom (Result Http.Error (Dict Int Int))
    | InitTick Time.Posix


type Message
    = LoadingMessage InitMessage
    | RunningMessage QaopMessage


type alias Flags =
    { rom : String
    , tape : String
    }


init : Flags -> ( Model, Cmd Message )
init data =
    ( Model (Loading (LoadingModel (RomURL data.rom) Nothing data.tape)) c_TICKTIME, romLoad data.rom )


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
    in
    folded2 |> Vector8.map (\row -> g [] row) |> Vector8.toList |> g []


screenDataNodeList : Bool -> Vector24 ScreenLine -> Vector24 (Svg Message)
screenDataNodeList flash screenLines =
    screenLines
        |> Vector24.indexedMap
            (\index line ->
                let
                    -- if flash is off (bit 7) on the whole line, use False for globalFlash value to help SVG caching
                    localFlash =
                        if (line.attrs |> Vector32.foldl (\x y -> Bitwise.or x y) 0x00 |> Bitwise.and 0x80) == 0x00 then
                            False

                        else
                            flash
                in
                line
                    |> Svg.Lazy.lazy3 mapScreenLineToSvg localFlash index
            )


svgNode : Z80Screen -> Bool -> Html Message
svgNode screen flash =
    let
        nodelist =
            screenDataNodeList flash

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
view model =
    case model.state of
        Loading _ ->
            div [] []

        Running qaopModel ->
            viewQaop qaopModel model.tickInterval


viewQaop : QaopModel -> Int -> Html Message
viewQaop model tickInterval =
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
        [ h2 [] [ text ("Refresh Interval " ++ (tickInterval |> String.fromInt) ++ "ms ") ]
        , div [ style "display" "flex", style "justify-content" "center" ]
            [ div []
                [ span [ id "cyclecount" ] [ text (String.fromInt model.count) ]
                , text " in "
                , span [ id "elapsed" ] [ text (model.elapsed_millis // 1000 |> String.fromInt) ]
                , text " sec, "
                , text time_disp
                , span [ id "hz" ] [ text speed ]
                , text " Hz"
                ]
            , button [ onClick (RunningMessage Pause) ]
                [ text
                    (if model.qaop.spectrum.paused then
                        "Unpause"

                     else
                        "Pause"
                    )
                ]
            , button [ onClick (RunningMessage Autoload), disabled load_disabled ]
                [ text "Load"
                ]
            ]
        , div
            [ tabindex 0
            , id "spectrum"
            , preventDefaultOn "keydown" (Decode.map alwaysPreventDefault keyDownDecoder)
            , preventDefaultOn "keyup" (Decode.map alwaysPreventDefault keyUpDecoder)
            ]
            [ Svg.Lazy.lazy2 svgNode screen model.globalFlash
            ]

        --,svg [style "height" "192px", style "width" "256px"] (List.indexedMap lineListToSvg lines |> List.concat)
        ]


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


type LoadResult
    = StillLoading LoadingModel
    | NowRunning QaopModel


updateLoading : InitMessage -> LoadingModel -> ( LoadResult, Cmd Message )
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
                        speccy =
                            Spectrum.constructor z80ROM

                        qaop =
                            { spectrum = speccy, keys = [], state = 0 }

                        qaopModel =
                            QaopModel qaop 0 0 posix False False
                    in
                    ( NowRunning qaopModel, tapLoad loadingModel.tapUrl )


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        LoadingMessage initMessage ->
            case model.state of
                Loading loadingModel ->
                    let
                        ( state, cmd ) =
                            updateLoading initMessage loadingModel
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


updateQaop : QaopMessage -> QaopModel -> ( QaopModel, Cmd QaopMessage )
updateQaop message model =
    case message of
        GotTAP result ->
            let
                qaop =
                    gotTap model.qaop result
            in
            ( { model | qaop = qaop, count = model.count + 1 }, Cmd.none )

        Tick posix ->
            let
                state =
                    if model.qaop.spectrum.paused then
                        { qaop = model.qaop, count = model.count, elapsed = 0 }

                    else
                        let
                            elapsed =
                                posixToMillis posix - posixToMillis model.time

                            q =
                                model.qaop |> run
                        in
                        { qaop = q, count = model.count + 1, elapsed = elapsed }
            in
            ( { model | count = state.count, elapsed_millis = model.elapsed_millis + state.elapsed, time = posix, qaop = state.qaop }, Cmd.none )

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

        FlipFlash _ ->
            ( { model | globalFlash = not model.globalFlash }, Cmd.none )


loadQuoteQuoteEnter =
    [ AutoChar 'j', AutoChar '"', AutoChar '"', AutoControl "Enter" ]


c_LOADING_KEY_DELAY =
    800


loadingCommands : List (Cmd QaopMessage)
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
subscriptions model =
    case model.state of
        Loading _ ->
            Time.every (model.tickInterval |> toFloat) (\posix -> LoadingMessage (InitTick posix))

        Running qaopModel ->
            qaopSubs qaopModel model.tickInterval |> Sub.map RunningMessage


qaopSubs : QaopModel -> Int -> Sub QaopMessage
qaopSubs model tickInterval =
    if model.qaop.spectrum.paused then
        Sub.none

    else
        let
            ticker =
                Time.every (tickInterval |> toFloat) (\posix -> Tick posix)

            flasher =
                Time.every (tickInterval * 16 |> toFloat) (\posix -> FlipFlash posix)
        in
        Sub.batch [ ticker, flasher ]


keyDownDecoder : Decode.Decoder Message
keyDownDecoder =
    Decode.map2 (\s b -> toKey s b |> RunningMessage) (Decode.field "key" Decode.string) (Decode.field "repeat" Decode.bool)


keyUpDecoder : Decode.Decoder Message
keyUpDecoder =
    Decode.map (\s -> toUnKey s |> RunningMessage) (Decode.field "key" Decode.string)


toKey : String -> Bool -> QaopMessage
toKey keyValue repeat =
    if repeat then
        KeyRepeat

    else
        case String.uncons keyValue of
            Just ( char, "" ) ->
                CharacterKeyDown char

            _ ->
                ControlKeyDown keyValue


toUnKey : String -> QaopMessage
toUnKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKeyUp char

        _ ->
            ControlKeyUp keyValue


tapLoad : String -> Cmd Message
tapLoad url =
    debugLog "loadTap"
        url
        Http.get
        { url = url
        , expect = Http.expectBytesResponse (\result -> RunningMessage (GotTAP result)) bytesToTap
        }


romLoad : String -> Cmd Message
romLoad url =
    debugLog "loadRom"
        url
        Http.get
        { url = url

        --, expect = Http.Detailed.expectBytes GotRom (array_decoder 16384 unsignedInt8)
        , expect = Http.expectBytesResponse (\result -> LoadingMessage (GotRom result)) bytesToRom
        }


gotTap : Qaop -> Result Http.Error (List Tapfile) -> Qaop
gotTap qaop result =
    case result of
        Ok value ->
            -- THe infinite recursion issue goes away if tape is a Dict rather than a List or Array
            -- it only happens when the VM starts running - if this is replaced with Cmd.none
            -- and we unpause manually, it crashes on the unpause - so something to do with Qaop.run
            -- and copying the Array
            { qaop | spectrum = qaop.spectrum |> new_tape value } |> run

        Err _ ->
            qaop



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


run : Qaop -> Qaop
run qaop =
    if qaop.spectrum.paused then
        { qaop | state = Bitwise.and qaop.state (complement 2), spectrum = qaop.spectrum |> Spectrum.pause 0x08 }

    else
        { qaop | spectrum = qaop.spectrum |> frames qaop.keys }


main : Program Flags Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
