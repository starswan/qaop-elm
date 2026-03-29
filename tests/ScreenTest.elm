module ScreenTest exposing (..)

import Expect
import Test exposing (..)
import Z80Screen exposing (foldBoolRunCounts)


suite : Test
suite =
    describe "things"
        [ describe "rawToLines"
            [ test "foldBoolRunCounts" <|
                \_ ->
                    let
                        a =
                            [ 0x80, 0x03 ] |> List.concatMap Z80Screen.intToBools
                    in
                    Expect.equal
                        [ { count = 1, value = True }
                        , { count = 13, value = False }
                        , { count = 2, value = True }
                        ]
                        (a |> List.foldl foldBoolRunCounts [] |> List.reverse)
            ]
        ]
