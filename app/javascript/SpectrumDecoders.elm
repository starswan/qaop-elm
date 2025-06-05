module SpectrumDecoders exposing (..)

import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (unsignedInt16)


spectrumUnsigned16Bit =
    unsignedInt16 LE
