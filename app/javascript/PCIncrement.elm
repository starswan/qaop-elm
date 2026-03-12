module PCIncrement exposing (..)


type PCIncrement
    = IncrementByOne
    | IncrementByTwo
    | IncrementByThree
    | IncrementByFour


pcIncrementToOffset : PCIncrement -> Int
pcIncrementToOffset pc_inc =
    case pc_inc of
        IncrementByOne ->
            1

        IncrementByTwo ->
            2

        IncrementByThree ->
            3

        IncrementByFour ->
            4
