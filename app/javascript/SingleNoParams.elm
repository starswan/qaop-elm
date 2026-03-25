module SingleNoParams exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import RegisterChange exposing (RegisterFlagChange(..))


singleWithNoParam : Dict Int ( RegisterFlagChange, InstructionDuration )
singleWithNoParam =
    Dict.fromList
        [ ( 0x00, ( RegChangeNoOp, FourTStates ) )

        --, ( 0x08, ( ExAfAfDash, FourTStates ) )
        -- case 0x40: break;
        , ( 0x40, ( RegChangeNoOp, FourTStates ) )

        -- case 0x49: break;
        , ( 0x49, ( RegChangeNoOp, FourTStates ) )

        -- case 0x52: break;
        , ( 0x52, ( RegChangeNoOp, FourTStates ) )

        -- case 0x5B: break;
        , ( 0x5B, ( RegChangeNoOp, FourTStates ) )

        -- case 0x64: break;
        , ( 0x64, ( RegChangeNoOp, FourTStates ) )

        -- case 0x6D: break;
        , ( 0x6D, ( RegChangeNoOp, FourTStates ) )

        -- case 0x7F: break;
        , ( 0x7F, ( RegChangeNoOp, FourTStates ) )
        , ( 0xC1, ( PopBC, TenTStates ) )
        , ( 0xC9, ( Ret, TenTStates ) )
        , ( 0xD1, ( PopDE, TenTStates ) )

        -- case 0xD9: exx(); break;
        --, ( 0xD9, ( Exx, FourTStates ) )
        , ( 0xE1, ( PopHL, TenTStates ) )
        , ( 0xF1, ( PopAF, TenTStates ) )

        --, ( 0xFB, ( EnableInterrupts, FourTStates ) )
        ]


singleNoParamCalls : Dict Int ( RegisterFlagChange, InstructionDuration )
singleNoParamCalls =
    Dict.fromList
        [ ( 0xC7, ( Rst 0x00, ElevenTStates ) )
        , ( 0xCF, ( Rst 0x08, ElevenTStates ) )
        , ( 0xD7, ( Rst 0x10, ElevenTStates ) )
        , ( 0xDF, ( Rst 0x18, ElevenTStates ) )
        , ( 0xE7, ( Rst 0x20, ElevenTStates ) )
        , ( 0xEF, ( Rst 0x28, ElevenTStates ) )
        , ( 0xF7, ( Rst 0x30, ElevenTStates ) )
        , ( 0xFF, ( Rst 0x38, ElevenTStates ) )
        ]


singleWithNoParamDD : Dict Int ( RegisterFlagChange, InstructionDuration )
singleWithNoParamDD =
    Dict.fromList
        [ ( 0xE1, ( PopIX, FourteenTStates ) )
        ]


singleWithNoParamFD : Dict Int ( RegisterFlagChange, InstructionDuration )
singleWithNoParamFD =
    Dict.fromList
        [ ( 0xE1, ( PopIY, FourteenTStates ) )
        ]
