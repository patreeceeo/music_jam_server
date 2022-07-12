module InstrumentTests exposing (..)

import Array
import Expect
import Instrument
import PortMessage
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
            Expect.equal expected
                (instrument.voices
                    |> Array.map (\v -> v.currentPitch)
                    |> Array.toList
                )
    ]


testMessagesForMappedChord : List Test
testMessagesForMappedChord =
    [ test "normal case" <|
        \_ ->
            let
                voices =
                    [ Instrument.initVoice [ 1.0, 2.0, 3.0 ]
                    , Instrument.initVoice [ 2.0, 3.0, 4.0 ]
                    , Instrument.initVoice [ 3.0, 4.0, 5.0 ]
                    ]

                chord =
                    [ Nothing
                    , Just 0
                    , Just 2
                    ]

                instrument =
                    Instrument.init voices
                        |> Instrument.setActiveChord chord

                expected =
                    [ PortMessage.PlaySoundRecord "acoustic-guitar" 1 2.0 4.2
                    , PortMessage.PlaySoundRecord "acoustic-guitar" 2 5.0 4.2
                    ]
            in
            Expect.equal expected (Instrument.chordNotes 4.2 instrument)
    ]
