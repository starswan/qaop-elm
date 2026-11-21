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
                        a =
                            intToRcList 0x00
                    in
                    Expect.equal
                        { firstCount = 8, initialValue = False, counts = [] }
                        a
            , test "03" <|
                \_ ->
                    let
                        --a : RunCount
                        --b : List RunCount
                        a =
                            intToRcList 0x03
                    in
                    Expect.equal
                        { firstCount = 6, initialValue = False, counts = [ 2 ] }
                        a
            , test "04" <|
                \_ ->
                    let
                        --a : RunCount
                        --b : List RunCount
                        a =
                            intToRcList 0x04
                    in
                    Expect.equal
                        { firstCount = 5, initialValue = False, counts = [ 1, 2 ] }
                        a
            , test "07" <|
                \_ ->
                    let
                        --a : RunCount
                        --b : List RunCount
                        a =
                            intToRcList 0x07
                    in
                    Expect.equal
                        { firstCount = 5, initialValue = False, counts = [ 3 ] }
                        a
            , test "80" <|
                \_ ->
                    let
                        --a : RunCount
                        --b : List RunCount
                        a =
                            intToRcList 0x80
                    in
                    Expect.equal
                        { firstCount = 1, initialValue = True, counts = [ 7 ] }
                        a
            ]
        ]
