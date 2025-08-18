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
    -- from https://en.wikipedia.org/wiki/ZX_Spectrum_graphic_modes
    "#000000"


c_BRIGHT_BLUE =
    "#0200FD"


c_BRIGHT_RED =
    "#FF0201"


c_BRIGHT_MAGENTA =
    "#FF02FD"


c_BRIGHT_GREEN =
    "#00FF1C"


c_BRIGHT_CYAN =
    "#02FFFF"


c_BRIGHT_YELLOW =
    "#FFFF1D"


c_WHITE =
    "#FFFFFF"


c_DULL_BLUE =
    "#0100CE"


c_DULL_RED =
    "#CF0100"


c_DULL_MAGENTA =
    "#CF01CE"


c_DULL_GREEN =
    "#00CF15"


c_DULL_CYAN =
    "#01CFCF"


c_DULL_YELLOW =
    "#CFCF15"


c_DULL_WHITE =
    "#CFCFCF"


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
        , ( 1, { value = BrightBlue, colour = c_BRIGHT_BLUE } )
        , ( 2, { value = BrightRed, colour = c_BRIGHT_RED } )
        , ( 3, { value = BrightMagenta, colour = c_BRIGHT_MAGENTA } )
        , ( 4, { value = BrightGreen, colour = c_BRIGHT_GREEN } )
        , ( 5, { value = BrightCyan, colour = c_BRIGHT_CYAN } )
        , ( 6, { value = BrightYellow, colour = c_BRIGHT_YELLOW } )
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
