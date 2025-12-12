module QaopModel exposing (..)

import Bitwise exposing (complement)
import Delay
import Dict
import Html exposing (Html, button, div, h2, span, text)
import Html.Attributes exposing (disabled, id, style, tabindex)
import Html.Events exposing (onClick, preventDefaultOn)
import Http
import Json.Decode as Decode
import Keyboard exposing (ctrlKeyDownEvent, ctrlKeyUpEvent, keyDownEvent, keyUpEvent)
import MessageHandler exposing (bytesToTap)
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
import Z80Screen exposing (ScreenColourRun, mapScreenLine)


type alias QaopModel =
    { qaop : Qaop
    , count : Int
    , elapsed_millis : Int
    , time : Time.Posix
    , loadPressed : Bool
    , globalFlash : Bool
    }


c_SCALEFACTOR =
    2


type AutoKey
    = AutoChar Char
    | AutoControl String


loadQuoteQuoteEnter =
    [ AutoChar 'j', AutoChar '"', AutoChar '"', AutoControl "Enter" ]


c_LOADING_KEY_DELAY =
    800


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


mapLineToSvg : Int -> ( Int, ScreenColourRun ) -> Svg QaopMessage
mapLineToSvg y_index ( start, linedata ) =
    line
        [ x1 (48 + start |> String.fromInt)
        , y1 (40 + y_index |> String.fromInt)
        , x2 ((48 + start + linedata.length) |> String.fromInt)
        , y2 (40 + y_index |> String.fromInt)
        , stroke (linedata.colour |> .colour)
        ]
        []


backgroundNode : Z80Screen -> Svg QaopMessage
backgroundNode screen =
    let
        -- border colour is never bright
        border_colour =
            borderColour screen.border
    in
    rect [ height "100%", width "100%", fill border_colour, rx "15" ] []


mapScreenLineToSvg : Bool -> Vector24.Index -> ScreenLine -> Svg QaopMessage
mapScreenLineToSvg flash index24 screenLine =
    let
        scrFolded : Vector8 (List ( Int, ScreenColourRun ))
        scrFolded =
            screenLine
                |> mapScreenLine flash

        folded2 : Vector8 (List (Svg QaopMessage))
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


screenDataNodeList : Bool -> Vector24 ScreenLine -> Vector24 (Svg QaopMessage)
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


svgNode : Z80Screen -> Bool -> Html QaopMessage
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


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


viewQaop : QaopModel -> Int -> Html QaopMessage
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
            [ Svg.Lazy.lazy2 svgNode screen model.globalFlash
            ]

        --,svg [style "height" "192px", style "width" "256px"] (List.indexedMap lineListToSvg lines |> List.concat)
        ]


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


keyDownDecoder : Decode.Decoder QaopMessage
keyDownDecoder =
    Decode.map2 toKey (Decode.field "key" Decode.string) (Decode.field "repeat" Decode.bool)


keyUpDecoder : Decode.Decoder QaopMessage
keyUpDecoder =
    Decode.map toUnKey (Decode.field "key" Decode.string)


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


tapLoad : String -> Cmd QaopMessage
tapLoad url =
    debugLog "loadTap"
        url
        Http.get
        { url = url
        , expect = Http.expectBytesResponse (\result -> GotTAP result) bytesToTap
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


run : Qaop -> Qaop
run qaop =
    if qaop.spectrum.paused then
        { qaop | state = Bitwise.and qaop.state (complement 2), spectrum = qaop.spectrum |> Spectrum.pause 0x08 }

    else
        { qaop | spectrum = qaop.spectrum |> frames qaop.keys }
