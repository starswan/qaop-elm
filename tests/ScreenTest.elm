module ScreenTest exposing (..)

import Expect
import Test exposing (..)
import Z80Screen exposing (foldBoolRunCounts, foldUp)


suite : Test
suite =
    describe "things"
        [ describe "lines"
            [ test "without dups" <|
                \_ ->
                    let
                        a =
                            [ { colour = 0x70, data = 0x76 } ]
                    in
                    Expect.equal [ { colour = 0x70, data = [ 0x76 ] } ] (a |> List.foldr foldUp [])
            , test "with dups" <|
                \_ ->
                    let
                        a =
                            [ { colour = 0x70, data = 0x76 }, { colour = 0x70, data = 0x71 }, { colour = 0x45, data = 0x87 } ]
                    in
                    Expect.equal [ { colour = 0x70, data = [ 0x76, 0x71 ] }, { colour = 0x45, data = [ 0x87 ] } ] (a |> List.foldr foldUp [])
            ]
        , describe "rawToLines"
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

            --,test "simple" <|
            --   \_ ->
            --      let
            --         -- colour is Ink 1 (bits 0-2) Paper 2 (bits 5-3) 1 dot, 7 blanks + 7 blanks, 1 dot
            --         a = [{colour=Bitwise.or 0x01 0x20, data=0x80}, {colour=Bitwise.or 0x01 0x20, data=0x03}]
            --      in
            --         Expect.equal [
            --         {start=0,length=1,colour=Blue,flash=False},
            --         {start=1,length=13, colour=Green, flash=False},
            --         {start=14,length=2, colour=Blue, flash=False}]
            --          (a |> Z80Screen.rawToLines)
            ]
        ]
