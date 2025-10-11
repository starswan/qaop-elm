module Z80Registers exposing (..)


type ChangeOneRegister
    = ChangeARegister
    | ChangeBRegister
    | ChangeCRegister
    | ChangeDRegister
    | ChangeERegister
    | ChangeHRegister
    | ChangeLRegister


type ChangeMainRegister
    = ChangeMainB
    | ChangeMainC
    | ChangeMainD
    | ChangeMainE
    | ChangeMainH
    | ChangeMainL
