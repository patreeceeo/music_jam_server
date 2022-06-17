module InstrumentTests exposing (..)

import Expect
import Instrument
import Test exposing (..)


suite : Test
suite =
    describe "Instrument"
        [ describe "mapActiveChord" testMapChord
        , describe "messagesForMappedChord" testMessagesForMappedChord
        ]


testMapChord : List Test
testMapChord =
    [ test "normal case" <|
        \_ ->
            let
                voices =
                    [ Instrument.initVoice [ 1.0, 2.0, 3.0 ]
                    , Instrument.initVoice [ 2.0, 3.0, 4.0 ]
                    , Instrument.initVoice [ 3.0, 4.0, 5.0 ]
                    ]

                chord =
                    [ Nothing, Just 1, Just 2 ]

                instrument =
                    Instrument.init voices
                        |> Instrument.setActiveChord chord

                expected =
                    [ Nothing
                    , Just 3.0
                    , Just 5.0
                    ]
            in
            Expect.equal expected (Instrument.mapActiveChord instrument)
    ]


testMessagesForMappedChord : List Test
testMessagesForMappedChord =
    [ test "normal case" <|
        \_ ->
            let
                chord =
                    [ Nothing
                    , Just 3.0
                    , Just 5.0
                    ]

                expected =
                    [ Nothing
                    , Just { soundId = "acoustic-guitar", voiceIndex = 1, pitch = 3.0, volume = 4.2 }
                    , Just { soundId = "acoustic-guitar", voiceIndex = 2, pitch = 5.0, volume = 4.2 }
                    ]
            in
            Expect.equal expected (Instrument.messagesForMappedChord chord 4.2)
    ]
