module Z80Registers exposing (..)


type ChangeMainRegister
    = ChangeMainB
    | ChangeMainC
    | ChangeMainD
    | ChangeMainE
    | ChangeMainH
    | ChangeMainL


type ChangeSingle
    = ChangeSingleH
    | ChangeSingleL


type CoreRegister
    = RegisterB
    | RegisterC
    | RegisterD
    | RegisterE
