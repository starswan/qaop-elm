module OpcodeTables exposing (..)

import CpuTimeCTime exposing (InstructionDuration)
import Dict exposing (Dict)
import DoubleWithRegisters exposing (DoubleWithRegisterChange, doubleWithRegisters, doubleWithRegistersIX, doubleWithRegistersIY)
import RegisterChange exposing (RegisterFlagChange(..), ThreeByteChange(..), TwoByteChange(..))
import SimpleFlagOps exposing (singleByteFlags, singleByteFlagsDD, singleByteFlagsFD)
import SimpleSingleByte exposing (singleByteMainRegs, singleByteMainRegsDD, singleByteMainRegsFD)
import SingleByteWithEnv exposing (singleByteZ80Env)
import SingleEnvWithMain exposing (singleEnvMainRegs, singleEnvMainRegsIX, singleEnvMainRegsIY)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters, singleByteMainAndFlagRegistersIX, singleByteMainAndFlagRegistersIY)
import SingleNoParams exposing (singleNoParamCalls, singleWithNoParam, singleWithNoParamDD, singleWithNoParamFD)
import SingleWith8BitParameter exposing (maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (TripleByteIndexChange, tripleByteWith16BitParam, tripleByteWith16BitParamDD, tripleByteWith16BitParamFD)
import TripleWithFlags exposing (triple16bitJumps)
import TripleWithMain exposing (tripleMainRegsIXFour, tripleMainRegsIXThree, tripleMainRegsIYFour, tripleMainRegsIYThree)


singleByteInstructions : Dict Int ( RegisterFlagChange, InstructionDuration )
singleByteInstructions =
    singleByteMainRegs
        |> Dict.union singleByteFlags
        |> Dict.union singleWithNoParam
        |> Dict.union singleNoParamCalls
        |> Dict.union (singleByteMainAndFlagRegisters |> Dict.map (\_ ( f, duration ) -> ( RegisterZ80Change f, duration )))
        |> Dict.union (singleEnvMainRegs |> Dict.map (\_ ( f, duration ) -> ( RegisterEnvMainChangeWithClockTime f, duration )))
        |> Dict.union (singleByteZ80Env |> Dict.map (\_ ( f, duration ) -> ( RegisterSingleByteEnv f, duration )))


twoByteInstructions : Dict Int ( Int -> TwoByteChange, InstructionDuration )
twoByteInstructions =
    singleWith8BitParam
        |> Dict.map (\_ ( f, duration ) -> ( \param -> TwoByte8Bit (f param), duration ))
        |> Dict.union (maybeRelativeJump |> Dict.map (\_ ( f, duration ) -> ( \param -> TwoByteJump (f param), duration )))
        |> Dict.union (doubleWithRegisters |> Dict.map (\_ ( f, duration ) -> ( \param -> TwoByteJump (f param), duration )))


threeByteInstructions : Dict Int ( Int -> ThreeByteChange, InstructionDuration )
threeByteInstructions =
    tripleByteWith16BitParam
        |> Dict.map (\_ ( f, duration ) -> ( \param -> ThreeBytePlain (f param), duration ))
        |> Dict.union (triple16bitJumps |> Dict.map (\_ ( f, duration ) -> ( \param -> ThreeByteFlags (f param), duration )))


singleByteMainFlagsRegsIY : Dict Int ( RegisterFlagChange, InstructionDuration )
singleByteMainFlagsRegsIY =
    singleByteFlagsFD
        |> Dict.union singleByteMainRegsFD
        |> Dict.union singleWithNoParamFD
        |> Dict.union (singleByteMainAndFlagRegistersIY |> Dict.map (\_ ( f, duration ) -> ( IndexedRegisterZ80Change f, duration )))
        |> Dict.union (singleEnvMainRegsIY |> Dict.map (\_ ( f, duration ) -> ( RegisterEnvMainChange f, duration )))


singleByteMainFlagsRegsIX : Dict Int ( RegisterFlagChange, InstructionDuration )
singleByteMainFlagsRegsIX =
    singleByteFlagsDD
        |> Dict.union singleByteMainRegsDD
        |> Dict.union singleWithNoParamDD
        |> Dict.union (singleByteMainAndFlagRegistersIX |> Dict.map (\_ ( f, duration ) -> ( IndexedRegisterZ80Change f, duration )))
        |> Dict.union (singleEnvMainRegsIX |> Dict.map (\_ ( f, duration ) -> ( RegisterEnvMainChange f, duration )))


twoByteWithRegistersIX : Dict Int ( Int -> DoubleWithRegisterChange, InstructionDuration )
twoByteWithRegistersIX =
    doubleWithRegistersIX |> Dict.union tripleMainRegsIXThree


threeByteWithRegistersIX : Dict Int ( Int -> TripleByteIndexChange, InstructionDuration )
threeByteWithRegistersIX =
    tripleByteWith16BitParamDD |> Dict.union tripleMainRegsIXFour


threeByteWithRegistersIY : Dict Int ( Int -> TripleByteIndexChange, InstructionDuration )
threeByteWithRegistersIY =
    tripleByteWith16BitParamFD |> Dict.union tripleMainRegsIYFour


twoByteWithRegistersIY : Dict Int ( Int -> DoubleWithRegisterChange, InstructionDuration )
twoByteWithRegistersIY =
    doubleWithRegistersIY |> Dict.union tripleMainRegsIYThree
