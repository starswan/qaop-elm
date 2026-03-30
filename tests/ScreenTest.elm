module ScreenTest exposing (..)

import Expect
import Test exposing (..)
import Z80Screen exposing (intToRcList)


suite : Test
suite =
    describe "things"
        [ describe "intToRcList"
            [ test "80" <|
                \_ ->
                    let
                        a =
                            intToRcList 0x80
                    in
                    Expect.equal
                        [ { count = 1, value = True }
                        , { count = 7, value = False }
                        ]
                        a
            , test "03" <|
                \_ ->
                    let
                        a =
                            intToRcList 0x03
                    in
                    Expect.equal
                        [ { count = 6, value = False }
                        , { count = 2, value = True }
                        ]
                        a
            ]
        ]
