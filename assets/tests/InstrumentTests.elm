module InstrumentTests exposing (..)

import Array
import Expect
import Instrument exposing (createInstrument, createInstrumentVoice, createMouseEvent, mouseOverVoice, setCurrentPitch, update, sendMessage, PlaySoundCmd, getPitchFromOffset)
import Json.Encode as Encode
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


type Msg
    = InstrumentMsg Instrument.Msg


suite : Test
suite =
    describe "Instrument module"
        [ test "setCurrentPitch" <|
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

                        mouseEvent = createMouseEvent 500 0 1

                        mouseOverVoiceMsg = mouseOverVoice 1 1000 mouseEvent
                    in
                    Instrument.view instrument 0 1000
                        |> Query.fromHtml
                        |> Query.find [ Selector.id "instrument-voice-1" ]
                        |> Event.simulate (Event.custom "mouseover" simulatedEventObject)
                        |> Event.expect mouseOverVoiceMsg
              , test "update model" <|
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

                        mouseEvent = createMouseEvent 500 0 1

                        ( updatedInstrument, _) = update (mouseOverVoice 1 1000 mouseEvent) instrument
                    in
                        Expect.equal (setCurrentPitch instrument 1 (getPitchFromOffset 500 1000)) updatedInstrument
              , test "send port message" <|
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

                        mouseEvent = createMouseEvent 500 0 1

                        ( _, playSoundCmd) = update (mouseOverVoice 1 1000 mouseEvent) instrument

                    in
                        Expect.equal playSoundCmd (sendMessage (PlaySoundCmd "acoustic-guitar" (getPitchFromOffset 500 1000) 40))

            ]
        ]

