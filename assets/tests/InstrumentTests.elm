module InstrumentTests exposing (..)

import Array
import Expect
import Instrument exposing (PortMessage(..), createInstrument, createInstrumentVoice, createMouseEvent, encodePortMessage, pitchAtOffset, mouseOverVoice, sendPortMessage, setCurrentPitch, update, fretDistance, fretIndex)
import Json.Encode as Encode
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


type Msg
    = InstrumentMsg Instrument.Msg


-- TODO make tests more focused
suite : Test
suite =
    describe "Instrument module"
        [
          test "fretDistance / fretIndex" <|
            \_ ->
              Expect.equal (fretIndex (fretDistance 13)) 13
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
        ]
