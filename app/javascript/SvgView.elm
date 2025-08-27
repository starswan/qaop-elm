module SvgView exposing (..)

import MessageHandler exposing (Message, c_SCALEFACTOR)
import ScreenStorage exposing (Z80Screen)
import SpectrumColour exposing (borderColour)
import Svg exposing (Svg, g, line, rect, svg)
import Svg.Attributes exposing (fill, height, rx, stroke, viewBox, width, x1, x2, y1, y2)
import Svg.Lazy exposing (lazy)
import Z80Screen exposing (ScreenColourRun, screenLines)


lineToSvg : Int -> ( Int, ScreenColourRun ) -> Svg Message
lineToSvg y_index ( start, linedata ) =
    line
        [ x1 (48 + start |> String.fromInt)
        , y1 (40 + y_index |> String.fromInt)
        , x2 ((48 + start + linedata.length) |> String.fromInt)
        , y2 (40 + y_index |> String.fromInt)
        , stroke (linedata.colour |> .colour)
        ]
        []


lineListToSvg : Int -> List ( Int, ScreenColourRun ) -> List (Svg Message)
lineListToSvg y_index linelist =
    linelist |> List.map (lineToSvg y_index)


lazySvgNode : Z80Screen -> Svg Message
lazySvgNode screen =
    lazy svgNode screen


svgNode : Z80Screen -> Svg Message
svgNode screen =
    svg
        [ height (272 * c_SCALEFACTOR |> String.fromInt)
        , width (352 * c_SCALEFACTOR |> String.fromInt)
        , viewBox "0 0 352 272"
        ]
        --<rect width="100%" height="100%" fill="green" />
        [ lazy backgroundNode screen

        --, g [] (screenDataNodes screen)
        , g [] (screen |> screenDataNodeList |> List.map (\l -> g [] l))
        ]


backgroundNode : Z80Screen -> Svg Message
backgroundNode screen =
    let
        -- border colour is never bright
        border_colour =
            borderColour screen.border
    in
    rect [ height "100%", width "100%", fill border_colour, rx "15" ] []


screenDataNodeList : Z80Screen -> List (List (Svg Message))
screenDataNodeList screen =
    let
        -- List (0-255) of List SCR
        screen1 : List (List ScreenColourRun)
        screen1 =
            screen
                |> screenLines

        screen2 : List (List ( Int, ScreenColourRun ))
        screen2 =
            screen1
                |> List.map (\scrList -> scrList |> List.foldl foldScr [] |> List.reverse)
    in
    screen2
        |> List.indexedMap lineListToSvg


foldScr : ScreenColourRun -> List ( Int, ScreenColourRun ) -> List ( Int, ScreenColourRun )
foldScr item list =
    case list of
        ( head, headScr ) :: tail ->
            ( head + headScr.length, item ) :: list

        _ ->
            List.singleton ( 0, item )
