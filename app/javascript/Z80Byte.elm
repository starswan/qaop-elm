module Z80Byte exposing (..)


type Z80Byte
    = Hex00
    | Hex01
    | Hex02
    | Hex03
    | Hex04
    | Hex05
    | Hex06
    | Hex07
    | Hex08
    | Hex09
    | Hex0A
    | Hex0B
    | Hex0C
    | Hex0D
    | Hex0E
    | Hex0F
    | Hex10
    | Hex11
    | Hex12
    | Hex13
    | Hex14
    | Hex15
    | Hex16
    | Hex17
    | Hex18
    | Hex19
    | Hex1A
    | Hex1B
    | Hex1C
    | Hex1D
    | Hex1E
    | Hex1F
    | Hex20
    | Hex21
    | Hex22
    | Hex23
    | Hex24
    | Hex25
    | Hex26
    | Hex27
    | Hex28
    | Hex29
    | Hex2A
    | Hex2B
    | Hex2C
    | Hex2D
    | Hex2E
    | Hex2F
    | Hex30
    | Hex31
    | Hex32
    | Hex33
    | Hex34
    | Hex35
    | Hex36
    | Hex37
    | Hex38
    | Hex39
    | Hex3A
    | Hex3B
    | Hex3C
    | Hex3D
    | Hex3E
    | Hex3F
    | Hex40
    | Hex41
    | Hex42
    | Hex43
    | Hex44
    | Hex45
    | Hex46
    | Hex47
    | Hex48
    | Hex49
    | Hex4A
    | Hex4B
    | Hex4C
    | Hex4D
    | Hex4E
    | Hex4F
    | Hex50
    | Hex51
    | Hex52
    | Hex53
    | Hex54
    | Hex55
    | Hex56
    | Hex57
    | Hex58
    | Hex59
    | Hex5A
    | Hex5B
    | Hex5C
    | Hex5D
    | Hex5E
    | Hex5F
    | Hex60
    | Hex61
    | Hex62
    | Hex63
    | Hex64
    | Hex65
    | Hex66
    | Hex67
    | Hex68
    | Hex69
    | Hex6A
    | Hex6B
    | Hex6C
    | Hex6D
    | Hex6E
    | Hex6F
    | Hex70
    | Hex71
    | Hex72
    | Hex73
    | Hex74
    | Hex75
    | Hex76
    | Hex77
    | Hex78
    | Hex79
    | Hex7A
    | Hex7B
    | Hex7C
    | Hex7D
    | Hex7E
    | Hex7F
    | Hex80
    | Hex81
    | Hex82
    | Hex83
    | Hex84
    | Hex85
    | Hex86
    | Hex87
    | Hex88
    | Hex89
    | Hex8A
    | Hex8B
    | Hex8C
    | Hex8D
    | Hex8E
    | Hex8F
    | Hex90
    | Hex91
    | Hex92
    | Hex93
    | Hex94
    | Hex95
    | Hex96
    | Hex97
    | Hex98
    | Hex99
    | Hex9A
    | Hex9B
    | Hex9C
    | Hex9D
    | Hex9E
    | Hex9F
    | HexA0
    | HexA1
    | HexA2
    | HexA3
    | HexA4
    | HexA5
    | HexA6
    | HexA7
    | HexA8
    | HexA9
    | HexAA
    | HexAB
    | HexAC
    | HexAD
    | HexAE
    | HexAF
    | HexB0
    | HexB1
    | HexB2
    | HexB3
    | HexB4
    | HexB5
    | HexB6
    | HexB7
    | HexB8
    | HexB9
    | HexBA
    | HexBB
    | HexBC
    | HexBD
    | HexBE
    | HexBF
    | HexC0
    | HexC1
    | HexC2
    | HexC3
    | HexC4
    | HexC5
    | HexC6
    | HexC7
    | HexC8
    | HexC9
    | HexCA
    | HexCB
    | HexCC
    | HexCD
    | HexCE
    | HexCF
    | HexD0
    | HexD1
    | HexD2
    | HexD3
    | HexD4
    | HexD5
    | HexD6
    | HexD7
    | HexD8
    | HexD9
    | HexDA
    | HexDB
    | HexDC
    | HexDD
    | HexDE
    | HexDF
    | HexE0
    | HexE1
    | HexE2
    | HexE3
    | HexE4
    | HexE5
    | HexE6
    | HexE7
    | HexE8
    | HexE9
    | HexEA
    | HexEB
    | HexEC
    | HexED
    | HexEE
    | HexEF
    | HexF0
    | HexF1
    | HexF2
    | HexF3
    | HexF4
    | HexF5
    | HexF6
    | HexF7
    | HexF8
    | HexF9
    | HexFA
    | HexFB
    | HexFC
    | HexFD
    | HexFE
    | HexFF


toInt : Z80Byte -> Int
toInt byte =
    case byte of
        Hex00 ->
            0x00

        Hex01 ->
            0x01

        Hex02 ->
            0x02

        Hex03 ->
            0x03

        Hex04 ->
            0x04

        Hex05 ->
            0x05

        Hex06 ->
            0x06

        Hex07 ->
            0x07

        Hex08 ->
            0x08

        Hex09 ->
            0x09

        Hex0A ->
            0x0A

        Hex0B ->
            0x0B

        Hex0C ->
            0x0C

        Hex0D ->
            0x0D

        Hex0E ->
            0x0E

        Hex0F ->
            0x0F

        Hex10 ->
            0x10

        Hex11 ->
            0x11

        Hex12 ->
            0x12

        Hex13 ->
            0x13

        Hex14 ->
            0x14

        Hex15 ->
            0x15

        Hex16 ->
            0x16

        Hex17 ->
            0x17

        Hex18 ->
            0x18

        Hex19 ->
            0x19

        Hex1A ->
            0x1A

        Hex1B ->
            0x1B

        Hex1C ->
            0x1C

        Hex1D ->
            0x1D

        Hex1E ->
            0x06

        Hex1F ->
            0x1E

        Hex20 ->
            0x20

        Hex21 ->
            0x21

        Hex22 ->
            0x22

        Hex23 ->
            0x23

        Hex24 ->
            0x24

        Hex25 ->
            0x25

        Hex26 ->
            0x26

        Hex27 ->
            0x27

        Hex28 ->
            0x28

        Hex29 ->
            0x29

        Hex2A ->
            0x2A

        Hex2B ->
            0x2B

        Hex2C ->
            0x2C

        Hex2D ->
            0x2D

        Hex2E ->
            0x2E

        Hex2F ->
            0x2F

        Hex30 ->
            0x30

        Hex31 ->
            0x31

        Hex32 ->
            0x32

        Hex33 ->
            0x33

        Hex34 ->
            0x34

        Hex35 ->
            0x35

        Hex36 ->
            0x36

        Hex37 ->
            0x37

        Hex38 ->
            0x38

        Hex39 ->
            0x39

        Hex3A ->
            0x3A

        Hex3B ->
            0x3B

        Hex3C ->
            0x3C

        Hex3D ->
            0x3D

        Hex3E ->
            0x3E

        Hex3F ->
            0x3F

        Hex40 ->
            0x40

        Hex41 ->
            0x41

        Hex42 ->
            0x42

        Hex43 ->
            0x43

        Hex44 ->
            0x44

        Hex45 ->
            0x45

        Hex46 ->
            0x46

        Hex47 ->
            0x47

        Hex48 ->
            0x48

        Hex49 ->
            0x49

        Hex4A ->
            0x4A

        Hex4B ->
            0x4B

        Hex4C ->
            0x4C

        Hex4D ->
            0x4D

        Hex4E ->
            0x4E

        Hex4F ->
            0x4F

        Hex50 ->
            0x50

        Hex51 ->
            0x51

        Hex52 ->
            0x52

        Hex53 ->
            0x53

        Hex54 ->
            0x54

        Hex55 ->
            0x55

        Hex56 ->
            0x56

        Hex57 ->
            0x57

        Hex58 ->
            0x58

        Hex59 ->
            0x59

        Hex5A ->
            0x5A

        Hex5B ->
            0x5B

        Hex5C ->
            0x5C

        Hex5D ->
            0x5D

        Hex5E ->
            0x5E

        Hex5F ->
            0x5F

        Hex60 ->
            0x60

        Hex61 ->
            0x61

        Hex62 ->
            0x62

        Hex63 ->
            0x63

        Hex64 ->
            0x64

        Hex65 ->
            0x65

        Hex66 ->
            0x66

        Hex67 ->
            0x67

        Hex68 ->
            0x68

        Hex69 ->
            0x69

        Hex6A ->
            0x6A

        Hex6B ->
            0x6B

        Hex6C ->
            0x6C

        Hex6D ->
            0x6D

        Hex6E ->
            0x6E

        Hex6F ->
            0x6F

        Hex70 ->
            0x70

        Hex71 ->
            0x71

        Hex72 ->
            0x72

        Hex73 ->
            0x73

        Hex74 ->
            0x74

        Hex75 ->
            0x75

        Hex76 ->
            0x76

        Hex77 ->
            0x77

        Hex78 ->
            0x78

        Hex79 ->
            0x79

        Hex7A ->
            0x7A

        Hex7B ->
            0x7B

        Hex7C ->
            0x7C

        Hex7D ->
            0x7D

        Hex7E ->
            0x7E

        Hex7F ->
            0x7F

        Hex80 ->
            0x80

        Hex81 ->
            0x81

        Hex82 ->
            0x82

        Hex83 ->
            0x83

        Hex84 ->
            0x84

        Hex85 ->
            0x85

        Hex86 ->
            0x86

        Hex87 ->
            0x87

        Hex88 ->
            0x88

        Hex89 ->
            0x89

        Hex8A ->
            0x8A

        Hex8B ->
            0x8B

        Hex8C ->
            0x8C

        Hex8D ->
            0x8D

        Hex8E ->
            0x8E

        Hex8F ->
            0x8F

        Hex90 ->
            0x90

        Hex91 ->
            0x91

        Hex92 ->
            0x92

        Hex93 ->
            0x93

        Hex94 ->
            0x94

        Hex95 ->
            0x95

        Hex96 ->
            0x96

        Hex97 ->
            0x97

        Hex98 ->
            0x98

        Hex99 ->
            0x99

        Hex9A ->
            0x9A

        Hex9B ->
            0x9B

        Hex9C ->
            0x9C

        Hex9D ->
            0x9D

        Hex9E ->
            0x9E

        Hex9F ->
            0x9F

        HexA0 ->
            0xA0

        HexA1 ->
            0xA1

        HexA2 ->
            0xA2

        HexA3 ->
            0xA3

        HexA4 ->
            0xA4

        HexA5 ->
            0xA5

        HexA6 ->
            0xA6

        HexA7 ->
            0xA7

        HexA8 ->
            0xA8

        HexA9 ->
            0xA9

        HexAA ->
            0xAA

        HexAB ->
            0xAB

        HexAC ->
            0xAC

        HexAD ->
            0xAD

        HexAE ->
            0xAE

        HexAF ->
            0xAF

        HexB0 ->
            0xB0

        HexB1 ->
            0xB1

        HexB2 ->
            0xB2

        HexB3 ->
            0xB3

        HexB4 ->
            0xB4

        HexB5 ->
            0xB5

        HexB6 ->
            0xB6

        HexB7 ->
            0xB7

        HexB8 ->
            0xB8

        HexB9 ->
            0xB9

        HexBA ->
            0xBA

        HexBB ->
            0xBB

        HexBC ->
            0xBC

        HexBD ->
            0xBD

        HexBE ->
            0xBE

        HexBF ->
            0xBF

        HexC0 ->
            0xC0

        HexC1 ->
            0xC1

        HexC2 ->
            0xC2

        HexC3 ->
            0xC3

        HexC4 ->
            0xC4

        HexC5 ->
            0xC5

        HexC6 ->
            0xC6

        HexC7 ->
            0xC7

        HexC8 ->
            0xC8

        HexC9 ->
            0xC9

        HexCA ->
            0xCA

        HexCB ->
            0xCB

        HexCC ->
            0xCC

        HexCD ->
            0xCD

        HexCE ->
            0xCE

        HexCF ->
            0xCF

        HexD0 ->
            0xD0

        HexD1 ->
            0xD1

        HexD2 ->
            0xD2

        HexD3 ->
            0xD3

        HexD4 ->
            0xD4

        HexD5 ->
            0xD5

        HexD6 ->
            0xD6

        HexD7 ->
            0xD7

        HexD8 ->
            0xD8

        HexD9 ->
            0xD9

        HexDA ->
            0xDA

        HexDB ->
            0xDB

        HexDC ->
            0xDC

        HexDD ->
            0xDD

        HexDE ->
            0xDE

        HexDF ->
            0xDF

        HexE0 ->
            0xE0

        HexE1 ->
            0xE1

        HexE2 ->
            0xE2

        HexE3 ->
            0xE3

        HexE4 ->
            0xE4

        HexE5 ->
            0xE5

        HexE6 ->
            0xE6

        HexE7 ->
            0xE7

        HexE8 ->
            0xE8

        HexE9 ->
            0xE9

        HexEA ->
            0xEA

        HexEB ->
            0xEB

        HexEC ->
            0xEC

        HexED ->
            0xED

        HexEE ->
            0xEE

        HexEF ->
            0xEF

        HexF0 ->
            0xF0

        HexF1 ->
            0xF1

        HexF2 ->
            0xF2

        HexF3 ->
            0xF3

        HexF4 ->
            0xF4

        HexF5 ->
            0xF5

        HexF6 ->
            0xF6

        HexF7 ->
            0xF7

        HexF8 ->
            0xF8

        HexF9 ->
            0xF9

        HexFA ->
            0xFA

        HexFB ->
            0xFB

        HexFC ->
            0xFC

        HexFD ->
            0xFD

        HexFE ->
            0xFE

        HexFF ->
            0xFF
