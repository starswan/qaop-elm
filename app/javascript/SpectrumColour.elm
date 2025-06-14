module SpectrumColour exposing (..)

import PlainColour exposing (PlainColour(..))


type SpectrumColourValue
    = DullBlack
    | DullBlue
    | DullRed
    | DullMagenta
    | DullGreen
    | DullCyan
    | DullYellow
    | DullWhite
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


c_DULL =
    "D7"


spectrumColour : PlainColour -> Bool -> SpectrumColour
spectrumColour value bright =
    if bright then
        --Dict.get value spectrumBrightColours |> withDefault { value = DullWhite, colour = c_DULL_WHITE }
        case value of
            Black ->
                { value = DullBlack, colour = c_BLACK }

            Blue ->
                { value = BrightBlue, colour = c_BLUE }

            Red ->
                { value = BrightRed, colour = c_RED }

            Magenta ->
                { value = BrightMagenta, colour = c_MAGENTA }

            Green ->
                { value = BrightGreen, colour = c_GREEN }

            Cyan ->
                { value = BrightCyan, colour = c_CYAN }

            Yellow ->
                { value = BrightYellow, colour = c_YELLOW }

            White ->
                { value = BrightWhite, colour = c_WHITE }

    else
        --Dict.get value spectrumColours |> withDefault { value = DullWhite, colour = c_DULL_WHITE }
        case value of
            Black ->
                { value = DullBlack, colour = c_BLACK }

            Blue ->
                { value = DullBlue, colour = c_DULL_BLUE }

            Red ->
                { value = DullRed, colour = c_DULL_RED }

            Magenta ->
                { value = DullMagenta, colour = c_DULL_MAGENTA }

            Green ->
                { value = DullGreen, colour = c_DULL_GREEN }

            Cyan ->
                { value = DullCyan, colour = c_DULL_CYAN }

            Yellow ->
                { value = DullYellow, colour = c_DULL_YELLOW }

            White ->
                { value = DullWhite, colour = c_DULL_WHITE }


c_DULL_BLUE =
    c_BLUE |> String.replace "FF" c_DULL


c_DULL_RED =
    c_RED |> String.replace "FF" c_DULL


c_DULL_MAGENTA =
    c_MAGENTA |> String.replace "FF" c_DULL


c_DULL_GREEN =
    c_GREEN |> String.replace "FF" c_DULL


c_DULL_CYAN =
    c_CYAN |> String.replace "FF" c_DULL


c_DULL_YELLOW =
    c_YELLOW |> String.replace "FF" c_DULL


c_DULL_WHITE =
    c_WHITE |> String.replace "FF" c_DULL
