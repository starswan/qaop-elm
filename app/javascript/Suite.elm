module Suite exposing (suite)

import Array
import Benchmark exposing (..)
import Bitwise
import Byte exposing (getBit)


suite : Benchmark
suite =
    let
        sampleArray =
            Array.initialize 2560 identity |> Array.indexedMap (\i v -> i)
    in
    describe "Colour"
        [ -- nest as many descriptions as you like
          describe "map"
            [ benchmark "checking each for 2 values" <|
                \_ ->
                    sampleArray
                        |> Array.map
                            (\raw_colour ->
                                let
                                    flashbright =
                                        raw_colour |> Bitwise.and 0xC0

                                    flash =
                                        flashbright == 0x80 || flashbright == 0xC0

                                    bright =
                                        flashbright == 0x40 || flashbright == 0xC0
                                in
                                ( flash, bright )
                            )
            , benchmark "case of" <|
                \_ ->
                    sampleArray
                        |> Array.map
                            (\raw_colour ->
                                case raw_colour |> Bitwise.and 0xC0 of
                                    0 ->
                                        ( False, False )

                                    0x40 ->
                                        ( False, True )

                                    0x80 ->
                                        ( True, False )

                                    _ ->
                                        ( True, True )
                            )
            , benchmark "original" <|
                \_ ->
                    sampleArray
                        |> Array.map
                            (\raw_colour ->
                                let
                                    colour_byte =
                                        Byte.fromInt raw_colour

                                    bright =
                                        colour_byte |> getBit 6

                                    flash =
                                        colour_byte |> getBit 7
                                in
                                ( bright, flash )
                            )
            ]
        ]



--(\dataIndex ->
--    let
--        dataRow =
--            memoryRow screenLine.data dataIndex
--
--        attr_row =
--            screenLine.attrs
--    in
--    Vector32.map2 (\data attr -> { colour = attr, data = data }) dataRow attr_row
--)
