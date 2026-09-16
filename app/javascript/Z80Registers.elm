module Z80Registers exposing (..)


type ChangeMainRegister
    = ChangeMainB
    | ChangeMainC
    | ChangeMainD
    | ChangeMainE
    | ChangeMainH
    | ChangeMainL


type CoreRegister
    = RegisterB
    | RegisterC
    | RegisterD
    | RegisterE
