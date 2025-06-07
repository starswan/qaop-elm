module UtilsTest exposing (..)

import Dict
import Expect
import Test exposing (..)
import Utils exposing (listToDict)


suite : Test
suite =
    describe "Utils"
        [ test "listToDict" <|
            \_ ->
                let
                    a =
                        listToDict [ 97, 98, 99, 100, 101, 102, 103, 104 ]
                in
                Expect.equal (Dict.fromList [ ( 0, 97 ), ( 1, 98 ), ( 2, 99 ), ( 3, 100 ), ( 4, 101 ), ( 5, 102 ), ( 6, 103 ), ( 7, 104 ) ]) a
        ]
