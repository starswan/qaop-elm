module SpectrumColour exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)


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


type alias ScreenAttribute =
    { flash : Bool
    , bright : Bool
    , paper : BorderColour
    , ink : BorderColour
    , paperColour : SpectrumColour
    , inkColour : SpectrumColour
    }


intToColour : Int -> BorderColour
intToColour int =
    case int of
        0 ->
            BorderBlack

        1 ->
            BorderBlue

        2 ->
            BorderRed

        3 ->
            BorderMagenta

        4 ->
            BorderGreen

        5 ->
            BorderCyan

        6 ->
            BorderYellow

        _ ->
            BorderWhite


intFromColour : BorderColour -> Int
intFromColour int =
    case int of
        BorderBlack ->
            0

        BorderBlue ->
            1

        BorderRed ->
            2

        BorderMagenta ->
            3

        BorderGreen ->
            4

        BorderCyan ->
            5

        BorderYellow ->
            6

        BorderWhite ->
            7


attributeFromInt : Int -> ScreenAttribute
attributeFromInt int =
    let
        flash =
            Bitwise.and int 0x80 /= 0

        paper =
            intToColour (int |> Bitwise.and 0x38 |> shiftRightBy 3)

        ink =
            intToColour (int |> Bitwise.and 0x07)

        bright =
            Bitwise.and int 0x40 /= 0

        ic =
            brightColourToSpectrumColour bright ink

        pc =
            brightColourToSpectrumColour bright paper
    in
    { flash = flash, bright = bright, paper = paper, ink = ink, inkColour = ic, paperColour = pc }


attributeToInt : ScreenAttribute -> Int
attributeToInt attribute =
    let
        flash =
            if attribute.flash then
                0x80

            else
                0x00

        bright =
            if attribute.bright then
                0x40

            else
                0x00

        paper =
            attribute.paper |> intFromColour |> shiftLeftBy 3

        ink =
            attribute.ink |> intFromColour
    in
    flash + bright + paper + ink


brightColourToSpectrumColour : Bool -> BorderColour -> SpectrumColour
brightColourToSpectrumColour bright colour =
    if bright then
        case colour of
            BorderBlack ->
                { value = Black, colour = c_BLACK }

            BorderBlue ->
                { value = BrightBlue, colour = c_BRIGHT_BLUE }

            BorderRed ->
                { value = BrightRed, colour = c_BRIGHT_RED }

            BorderMagenta ->
                { value = BrightMagenta, colour = c_BRIGHT_MAGENTA }

            BorderGreen ->
                { value = BrightGreen, colour = c_BRIGHT_GREEN }

            BorderCyan ->
                { value = BrightCyan, colour = c_BRIGHT_CYAN }

            BorderYellow ->
                { value = BrightYellow, colour = c_BRIGHT_YELLOW }

            BorderWhite ->
                { value = BrightWhite, colour = c_WHITE }

    else
        case colour of
            BorderBlack ->
                { value = Black, colour = c_BLACK }

            BorderBlue ->
                { value = Blue, colour = c_DULL_BLUE }

            BorderRed ->
                { value = Red, colour = c_DULL_RED }

            BorderMagenta ->
                { value = Magenta, colour = c_DULL_MAGENTA }

            BorderGreen ->
                { value = Green, colour = c_DULL_GREEN }

            BorderCyan ->
                { value = Cyan, colour = c_DULL_CYAN }

            BorderYellow ->
                { value = Yellow, colour = c_DULL_YELLOW }

            BorderWhite ->
                { value = White, colour = c_DULL_WHITE }


attributeAndBitToColour : Bool -> ScreenAttribute -> Bool -> SpectrumColour
attributeAndBitToColour globalFlash screenAttr bitValue =
    let
        value =
            if screenAttr.flash && globalFlash then
                not bitValue

            else
                bitValue
    in
    if value then
        screenAttr.inkColour

    else
        screenAttr.paperColour
