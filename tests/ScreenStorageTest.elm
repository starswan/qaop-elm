module ScreenStorageTest exposing (..)

import Expect
import ScreenStorage exposing (calcDataOffset, calcVectorDataOffset)
import Test exposing (Test, describe, test)
import Vector24
import Vector8


suite : Test
suite =
    describe "screen storage"
        [ describe "calcDataOffset"
            [ test "zero through 7" <|
                \_ ->
                    let
                        result =
                            List.range 0 7 |> List.map (\x -> calcDataOffset x)
                    in
                    Expect.equal [ 0, 8, 16, 24, 32, 40, 48, 56 ] result
            , test "8 to 15" <|
                \_ ->
                    let
                        result =
                            List.range 8 15 |> List.map (\x -> calcDataOffset x)
                    in
                    Expect.equal [ 1, 9, 17, 25, 33, 41, 49, 57 ] result
            , test "16 to 23" <|
                \_ ->
                    let
                        result =
                            List.range 16 23 |> List.map (\x -> calcDataOffset x)
                    in
                    Expect.equal [ 2, 10, 18, 26, 34, 42, 50, 58 ] result
            , test "24 to 31" <|
                \_ ->
                    let
                        result =
                            List.range 24 31 |> List.map (\x -> calcDataOffset x)
                    in
                    Expect.equal [ 3, 11, 19, 27, 35, 43, 51, 59 ] result
            , test "56 to 63" <|
                \_ ->
                    let
                        result =
                            List.range 56 63 |> List.map (\x -> calcDataOffset x)
                    in
                    Expect.equal [ 7, 15, 23, 31, 39, 47, 55, 63 ] result
            , test "64 to 71" <|
                \_ ->
                    let
                        result =
                            List.range 64 71 |> List.map (\x -> calcDataOffset x)
                    in
                    Expect.equal [ 64, 72, 80, 88, 96, 104, 112, 120 ] result
            , test "191" <|
                \_ ->
                    let
                        result =
                            calcDataOffset 191
                    in
                    Expect.equal 191 result
            ]
        , describe "calcVectorDataOffset"
            [ test "zero through7" <|
                \_ ->
                    let
                        --1st 8 rows of data map to rows 0-7 on the screen (first line)
                        result =
                            List.range 0 7 |> List.map (\x -> calcVectorDataOffset x)
                    in
                    Expect.equal
                        [ ( Vector24.Index0, Vector8.Index0 )
                        , ( Vector24.Index1, Vector8.Index0 )
                        , ( Vector24.Index2, Vector8.Index0 )
                        , ( Vector24.Index3, Vector8.Index0 )
                        , ( Vector24.Index4, Vector8.Index0 )
                        , ( Vector24.Index5, Vector8.Index0 )
                        , ( Vector24.Index6, Vector8.Index0 )
                        , ( Vector24.Index7, Vector8.Index0 )
                        ]
                        result
            ]
        ]
