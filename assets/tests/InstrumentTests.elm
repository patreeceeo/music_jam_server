module InstrumentTests exposing (..)

import Array
import Expect
import Instrument exposing (PathSegment, PortMessage(..), createInstrument, createInstrumentVoice, createMouseEvent, encodePortMessage, fretDistance, fretIndex, mouseOverVoice, pitchAtOffset, sendPortMessage, setCurrentPitch, update, viewStringAnimationValues)
import Json.Encode as Encode
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector



-- TODO make tests more focused


fretIndexes : List Int
fretIndexes =
    List.range 0 24


fretPlacementTests : List Test
fretPlacementTests =
    List.map
        (\index ->
            test ("for " ++ String.fromInt index) <|
                \_ ->
                    Expect.equal (fretIndex (fretDistance index)) index
        )
        fretIndexes


suite : Test
suite =
    describe "Instrument module"
        [ describe "fret placement" fretPlacementTests
        , test "setCurrentPitch" <|
            \_ ->
                let
                    voice1 =
                        createInstrumentVoice [ 1.0, 2.0, 3.0 ]

                    voice2 =
                        createInstrumentVoice [ 1.0, 2.0, 3.0 ]

                    modifiedVoice2 =
                        { voice2 | currentPitch = 42 }

                    voice3 =
                        createInstrumentVoice [ 1.0, 2.0, 3.0 ]

                    instrument =
                        createInstrument [ voice1, voice2, voice3 ]
                in
                Expect.equal
                    { voices = Array.fromList [ voice1, modifiedVoice2, voice3 ]
                    }
                    (setCurrentPitch instrument 1 42)
        , test "getPitchFromOffset" <|
            \_ ->
                let
                    voice1 =
                        createInstrumentVoice (List.map toFloat (List.range 0 24))

                    instrument =
                        createInstrument [ voice1 ]

                    getPitchResult =
                        pitchAtOffset 500 1000 instrument 0
                in
                case getPitchResult of
                    Ok pitch ->
                        Expect.equal 8 pitch

                    Err errMsg ->
                        Expect.fail errMsg
        , describe "MouseOverVoice event"
            [ test "generation" <|
                \_ ->
                    let
                        voice1 =
                            createInstrumentVoice [ 1.0, 2.0, 3.0 ]

                        voice2 =
                            createInstrumentVoice [ 1.0, 2.0, 3.0 ]

                        voice3 =
                            createInstrumentVoice [ 1.0, 2.0, 3.0 ]

                        instrument =
                            createInstrument [ voice1, voice2, voice3 ]

                        simulatedEventObject =
                            Encode.object
                                [ ( "offsetX", Encode.int 500 )
                                , ( "offsetY", Encode.int 0 )
                                , ( "buttons", Encode.int 1 )
                                ]

                        mouseEvent =
                            createMouseEvent 500 0 1

                        mouseOverVoiceMsg =
                            mouseOverVoice 1 1000 mouseEvent
                    in
                    Instrument.view instrument 0 1000
                        |> Query.fromHtml
                        |> Query.find [ Selector.id "instrument-voice-1" ]
                        |> Event.simulate (Event.custom "mouseover" simulatedEventObject)
                        |> Event.expect mouseOverVoiceMsg
            , test "update model" <|
                \_ ->
                    -- TODO this test needs to be broken up?
                    let
                        voice1 =
                            createInstrumentVoice (List.map toFloat (List.range 0 24))

                        voice2 =
                            createInstrumentVoice (List.map toFloat (List.range 0 24))

                        voice3 =
                            createInstrumentVoice (List.map toFloat (List.range 0 24))

                        instrument =
                            createInstrument [ voice1, voice2, voice3 ]

                        mouseEvent =
                            createMouseEvent 500 0 1

                        ( updatedInstrument, _ ) =
                            update (mouseOverVoice 1 1000 mouseEvent) instrument

                        getPitchResult =
                            pitchAtOffset 500 1000 instrument 1
                    in
                    case getPitchResult of
                        Ok pitch ->
                            Expect.equal (setCurrentPitch instrument 1 pitch) updatedInstrument

                        Err errMsg ->
                            Expect.fail errMsg
            , test "send port message" <|
                \_ ->
                    let
                        voice1 =
                            createInstrumentVoice (List.map toFloat (List.range 0 24))

                        voice2 =
                            createInstrumentVoice (List.map toFloat (List.range 0 24))

                        voice3 =
                            createInstrumentVoice (List.map toFloat (List.range 0 24))

                        instrument =
                            createInstrument [ voice1, voice2, voice3 ]

                        mouseEvent =
                            createMouseEvent 500 0 1

                        ( _, playSoundCmd ) =
                            update (mouseOverVoice 1 1000 mouseEvent) instrument

                        getPitchResult =
                            pitchAtOffset 500 1000 instrument 1
                    in
                    case getPitchResult of
                        Ok pitch ->
                            Expect.equal playSoundCmd (sendPortMessage (encodePortMessage (PlaySound { soundId = "acoustic-guitar", pitch = pitch, volume = 40 })))

                        Err errMsg ->
                            Expect.fail errMsg
            ]
        , test "viewStringAnimationValues" <|
            \_ ->
                let
                    activeFretX =
                        5

                    stringY =
                        13

                    instW =
                        16

                    period =
                        4

                    amplitude =
                        4

                    actual =
                        viewStringAnimationValues activeFretX stringY instW period amplitude

                    expected : List (List PathSegment)
                    expected =
                        [ [ PathSegment "M" [ [ 0, 13 ], [ 5, 13 ] ]
                          , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
                          , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
                          , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
                          , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
                          , PathSegment "q" [ [ 2, 2 ], [ 4, 0 ] ]
                          , PathSegment "t" [ [ 4, 0 ] ]
                          , PathSegment "t" [ [ 4, 0 ] ]
                          ]
                        , [ PathSegment "M" [ [ 0, 13 ], [ 5, 13 ] ]
                          , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
                          , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
                          , PathSegment "q" [ [ 2, 2 ], [ 4, 0 ] ]
                          , PathSegment "q" [ [ 2, -2 ], [ 4, 0 ] ]
                          , PathSegment "q" [ [ 2, 2 ], [ 4, 0 ] ]
                          , PathSegment "t" [ [ 4, 0 ] ]
                          , PathSegment "t" [ [ 4, 0 ] ]
                          ]
                        , [ PathSegment "M" [ [ 0, 13 ], [ 5, 13 ] ]
                          , PathSegment "q" [ [ 2, 2 ], [ 4, 0 ] ]
                          , PathSegment "q" [ [ 2, -2 ], [ 4, 0 ] ]
                          , PathSegment "q" [ [ 2, 2 ], [ 4, 0 ] ]
                          , PathSegment "q" [ [ 2, -2 ], [ 4, 0 ] ]
                          , PathSegment "q" [ [ 2, 2 ], [ 4, 0 ] ]
                          , PathSegment "t" [ [ 4, 0 ] ]
                          , PathSegment "t" [ [ 4, 0 ] ]
                          ]
                        ]
                in
                Expect.equal expected actual
        ]
