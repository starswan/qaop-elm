module Int8Test exposing (..)

import Expect
import Int8
import Test exposing (..)


suite : Test
suite =
    describe "Int8"
        -- Nest as many descriptions as you like.
        [ test "add" <|
            \_ ->
                1
                    |> Int8.fromInt
                    |> Int8.add 2
                    |> Expect.equal (Int8.fromInt 3)
        ]
