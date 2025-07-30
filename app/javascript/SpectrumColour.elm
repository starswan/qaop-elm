module SpectrumColour exposing (..)

import Dict
import Maybe exposing (withDefault)


type BorderColour
    = BorderBlack
    | BorderBlue
    | BorderRed
    | BorderMagenta
    | BorderGreen
    | BorderCyan
    | BorderYellow
    | BorderWhite


type SpectrumColourValue
    = Black
    | Blue
    | Red
    | Magenta
    | Green
    | Cyan
    | Yellow
    | White
    | BrightBlue
    | BrightRed
    | BrightMagenta
    | BrightGreen
    | BrightCyan
    | BrightYellow
    | BrightWhite


type alias SpectrumColour =
    { value : SpectrumColourValue
    , colour : String
    }


c_BLACK =
    "#000000"


c_BLUE =
    "#0000FF"


c_RED =
    "#FF0000"


c_MAGENTA =
    "#FF00FF"


c_GREEN =
    "#00FF00"


c_CYAN =
    "#00FFFF"


c_YELLOW =
    "#FFFF00"


c_WHITE =
    "#FFFFFF"


c_DULL_BLUE =
    "#0000D7"


c_DULL_RED =
    "#D70000"


c_DULL_MAGENTA =
    "#D700D7"


c_DULL_GREEN =
    "#00D700"


c_DULL_CYAN =
    "#00D7D7"


c_DULL_YELLOW =
    "#D7D700"


c_DULL_WHITE =
    "#D7D7D7"


spectrumColours =
    Dict.fromList
        [ ( 0, { value = Black, colour = c_BLACK } )
        , ( 1, { value = Blue, colour = c_DULL_BLUE } )
        , ( 2, { value = Red, colour = c_DULL_RED } )
        , ( 3, { value = Magenta, colour = c_DULL_MAGENTA } )
        , ( 4, { value = Green, colour = c_DULL_GREEN } )
        , ( 5, { value = Cyan, colour = c_DULL_CYAN } )
        , ( 6, { value = Yellow, colour = c_DULL_YELLOW } )
        , ( 7, { value = White, colour = c_DULL_WHITE } )
        ]


spectrumBrightColours =
    Dict.fromList
        [ ( 0, { value = Black, colour = c_BLACK } )
        , ( 1, { value = BrightBlue, colour = c_BLUE } )
        , ( 2, { value = BrightRed, colour = c_RED } )
        , ( 3, { value = BrightMagenta, colour = c_MAGENTA } )
        , ( 4, { value = BrightGreen, colour = c_GREEN } )
        , ( 5, { value = BrightCyan, colour = c_CYAN } )
        , ( 6, { value = BrightYellow, colour = c_YELLOW } )
        , ( 7, { value = BrightWhite, colour = c_WHITE } )
        ]


borderColour : BorderColour -> String
borderColour border =
    -- borderColours are never bright
    case border of
        BorderBlack ->
            c_BLACK

        BorderBlue ->
            c_DULL_BLUE

        BorderRed ->
            c_DULL_RED

        BorderMagenta ->
            c_DULL_MAGENTA

        BorderGreen ->
            c_DULL_GREEN

        BorderCyan ->
            c_DULL_CYAN

        BorderYellow ->
            c_DULL_YELLOW

        BorderWhite ->
            c_DULL_WHITE


spectrumColour : Int -> Bool -> SpectrumColour
spectrumColour value bright =
    if bright then
        Dict.get value spectrumBrightColours |> withDefault { value = White, colour = c_DULL_WHITE }

    else
        Dict.get value spectrumColours |> withDefault { value = White, colour = c_DULL_WHITE }
