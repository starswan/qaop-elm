module ScreenTest exposing (..)

import Expect
import Test exposing (..)
import Z80Screen exposing (RunCount, intToRcList)


suite : Test
suite =
    describe "things"
        [ describe "intToRcList"
            [ test "00" <|
                \_ ->
                    let
                        --a : RunCount
                        --b : List RunCount
                        ( a, b ) =
                            intToRcList 0x00
                    in
                    Expect.equal
                        [ { count = 8, value = False }
                        ]
                        (a :: b)
            , test "03" <|
                \_ ->
                    let
                        --a : RunCount
                        --b : List RunCount
                        ( a, b ) =
                            intToRcList 0x03
                    in
                    Expect.equal
                        [ { count = 6, value = False }
                        , { count = 2, value = True }
                        ]
                        (a :: b)
            , test "04" <|
                \_ ->
                    let
                        --a : RunCount
                        --b : List RunCount
                        ( a, b ) =
                            intToRcList 0x04
                    in
                    Expect.equal
                        [ { count = 5, value = False }
                        , { count = 1, value = True }
                        , { count = 2, value = False }
                        ]
                        (a :: b)
            , test "07" <|
                \_ ->
                    let
                        --a : RunCount
                        --b : List RunCount
                        ( a, b ) =
                            intToRcList 0x07
                    in
                    Expect.equal
                        [ { count = 5, value = False }
                        , { count = 3, value = True }
                        ]
                        (a :: b)
            , test "80" <|
                \_ ->
                    let
                        --a : RunCount
                        --b : List RunCount
                        ( a, b ) =
                            intToRcList 0x80
                    in
                    Expect.equal
                        [ { count = 1, value = True }
                        , { count = 7, value = False }
                        ]
                        (a :: b)
            ]
        ]
