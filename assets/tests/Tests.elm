module Tests exposing (..)

import Array
import Expect
import Instrument exposing (init, initVoice, fretDistance, fretIndex, pitchAtOffset, setCurrentPitch)
import Json.Encode as Encode
import Main exposing (Model, update, view)
import MouseEvent
import OperatingSystem as OS
import PortMessage
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import UserInterfaces as UI
import Utils exposing (PathSegment)
import Instrument exposing (setCurrentVolume)



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
                    Expect.equal (fretIndex (fretDistance index + 0.00001)) index
        )
        fretIndexes


wrapInstrument : Instrument.Model -> Model
wrapInstrument instrument =
    { instrument = Just instrument
    , os = OS.init 1000
    }


suite : Test
suite =
    describe "Instrument module"
        [ describe "fret placement" fretPlacementTests
        , test "setCurrentPitch" <|
            \_ ->
                let
                    voice1 =
                        initVoice [ 1.0, 2.0, 3.0 ]

                    voice2 =
                        initVoice [ 1.0, 2.0, 3.0 ]

                    modifiedVoice2 =
                        { voice2 | currentPitch = 42 }

                    voice3 =
                        initVoice [ 1.0, 2.0, 3.0 ]

                    instrument =
                        init [ voice1, voice2, voice3 ]
                in
                Expect.equal
                    { voices = Array.fromList [ voice1, modifiedVoice2, voice3 ]
                    }
                    (setCurrentPitch 42 1 instrument)
        , test "getPitchFromOffset" <|
            \_ ->
                let
                    voice1 =
                        initVoice (List.map toFloat (List.range 0 24))

                    instrument =
                        init [ voice1 ]

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
                            initVoice [ 1.0, 2.0, 3.0 ]

                        voice2 =
                            initVoice [ 1.0, 2.0, 3.0 ]

                        voice3 =
                            initVoice [ 1.0, 2.0, 3.0 ]

                        instrument =
                            init [ voice1, voice2, voice3 ]

                        simulatedEventObject =
                            Encode.object
                                [ ( "offsetX", Encode.int 500 )
                                , ( "offsetY", Encode.int 0 )
                                , ( "buttons", Encode.int 1 )
                                ]

                        mouseEvent =
                            MouseEvent.create 500 0 1

                        mouseOverVoiceMsg =
                            Main.MouseOverVoice 1 mouseEvent
                    in
                    view (wrapInstrument instrument)
                        |> Query.fromHtml
                        |> Query.find [ Selector.id "instrument-voice-1" ]
                        |> Event.simulate (Event.custom "mouseover" simulatedEventObject)
                        |> Event.expect mouseOverVoiceMsg
            , test "update model" <|
                \_ ->
                    -- TODO this test needs to be broken up?
                    let
                        voice1 =
                            initVoice (List.map toFloat (List.range 0 24))

                        voice2 =
                            initVoice (List.map toFloat (List.range 0 24))

                        voice3 =
                            initVoice (List.map toFloat (List.range 0 24))

                        instrument =
                            init [ voice1, voice2, voice3 ]

                        mouseEvent =
                            MouseEvent.create 500 0 1

                        ( updatedModel, _ ) =
                            update (Main.MouseOverVoice 1 mouseEvent) (wrapInstrument instrument)

                        getPitchResult =
                            pitchAtOffset 500 1000 instrument 1
                    in
                    case getPitchResult of
                        Ok pitch ->
                            Expect.equal (Just (instrument |> setCurrentPitch pitch 1 |> setCurrentVolume 0.5 1)) updatedModel.instrument

                        Err errMsg ->
                            Expect.fail errMsg
            , test "send port message" <|
                \_ ->
                    let
                        voice1 =
                            initVoice (List.map toFloat (List.range 0 24))

                        voice2 =
                            initVoice (List.map toFloat (List.range 0 24))

                        voice3 =
                            initVoice (List.map toFloat (List.range 0 24))

                        instrument =
                            init [ voice1, voice2, voice3 ]

                        mouseEvent =
                            MouseEvent.create 500 0 1

                        ( _, playSoundCmd ) =
                            update (Main.MouseOverVoice 1 mouseEvent) (wrapInstrument instrument)

                        getPitchResult =
                            pitchAtOffset 500 1000 instrument 1
                    in
                    case getPitchResult of
                        Ok pitch ->
                            Expect.equal playSoundCmd (PortMessage.send (PortMessage.PlaySound { soundId = "acoustic-guitar", voiceIndex = 1, pitch = pitch, volume = 0.5 }))

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
                        UI.viewStringAnimationValues activeFretX stringY instW period amplitude

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
