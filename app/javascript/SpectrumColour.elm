module SpectrumColour exposing (..)

import Array
import Color exposing (Color)
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


borders =
    Array.fromList [ BorderBlack, BorderBlue, BorderRed, BorderMagenta, BorderGreen, BorderCyan, BorderYellow, BorderWhite ]


intToBorderColour : Int -> BorderColour
intToBorderColour value =
    borders |> Array.get value |> withDefault BorderWhite


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
    , colour : Color
    }


c_BLACK =
    -- from https://en.wikipedia.org/wiki/ZX_Spectrum_graphic_modes
    --"#000000"
    Color.black


c_BRIGHT_BLUE =
    --"#0200FD"
    Color.rgb255 0x02 0x00 0xFD


c_BRIGHT_RED =
    --"#FF0201"
    Color.lightRed


c_BRIGHT_MAGENTA =
    --"#FF02FD"
    Color.rgb255 0xFF 0x02 0xFD


c_BRIGHT_GREEN =
    --"#00FF1C"
    Color.lightGreen


c_BRIGHT_CYAN =
    --"#02FFFF"
    Color.rgb255 0x02 0xFF 0xFF


c_BRIGHT_YELLOW =
    --"#FFFF1D"
    Color.lightYellow


c_WHITE =
    --"#FFFFFF"
    Color.white


c_DULL_BLUE =
    --"#0100CE"
    Color.rgb255 0x01 0x00 0xCE


c_DULL_RED =
    --"#CF0100"
    Color.darkRed


c_DULL_MAGENTA =
    --"#CF01CE"
    Color.rgb255 0xCF 0x01 0xCE


c_DULL_GREEN =
    --"#00CF15"
    Color.darkGreen


c_DULL_CYAN =
    --"#01CFCF"
    Color.rgb255 0x01 0xCF 0xCF


c_DULL_YELLOW =
    --"#CFCF15"
    Color.darkYellow


c_DULL_WHITE =
    --"#CFCFCF"
    Color.rgb255 0xCF 0xCF 0xCF


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


borderColour : BorderColour -> Color
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
