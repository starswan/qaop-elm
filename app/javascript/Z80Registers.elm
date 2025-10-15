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


type EightBitMain
    = RegisterB
    | RegisterC
    | RegisterD
    | RegisterE


type Single8BitChange
    = NewBRegister Int
    | NewCRegister Int
    | NewDRegister Int
    | NewERegister Int
