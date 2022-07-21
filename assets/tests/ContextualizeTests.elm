module ContextualizeTests exposing (suite)

import Chord exposing (Names(..))
import CommonTypes exposing (Routes(..), Selectors)
import Expect
import Flags
import Instrument
import Json.Encode as E
import KbdEvent
import Main exposing (Context(..), Model, contextualize, init)
import Message exposing (Message(..))
import OperatingSystem
import Test exposing (..)
import Time
import Url
import User.Interface exposing (UserAction(..))
import Utils exposing (TestableNavKey(..))
import Router


flags : Flags.Model
flags =
    { clientId = "sponge"
    , screenWidth = 320
    , baseHref = "://www.com"
    , instrument =
        Instrument.init
            [ Instrument.initVoice [ 22, 33, 44, 55, 66 ]
            ]
    , nextRoute = SelectChordRoute
    }


model : Model
model =
    Tuple.first (init (Ok flags) (Url.fromString "://www.com/b") TestNavKey)


suite : Test
suite =
    describe "contextualize" contextualizeTests



-- Can get rid of selectors?


selectors : Selectors
selectors =
    Main.bindSelectors model



-- TODO create model for tests using JSON decoder?


contextualizeTests : List Test
contextualizeTests =
    [ test "ReceivePortMessage" <|
        \_ ->
            let
                msg =
                    ReceivePortMessage "bob" (E.int 0)

                result =
                    contextualize msg model
            in
            Expect.equal result (WithClientId "sponge")
    , test "KeyDown with enter key on any route" <|
        \_ ->
            let
                msg =
                    KeyDown { key = KbdEvent.KeyEnter }

                result =
                    contextualize msg model
            in
            Expect.equal result (WithRoute SelectChordRoute)
    , test "KeyUp with play chord key" <|
        \_ ->
            let
                msg1 =
                    Message.KeyDown { key = KbdEvent.KeySpace }

                msg2 =
                    Message.AnimationFrame (Time.millisToPosix 15)

                msg3 =
                    KeyUp { key = KbdEvent.KeySpace }

                model2 =
                    { model | os = Tuple.first (OperatingSystem.update msg1 model.os selectors) }

                model3 =
                    { model2 | os = Tuple.first (OperatingSystem.update msg2 model2.os selectors) }

                result =
                    contextualize msg3 model3
            in
            Expect.equal result (WithRouteAndVolume NotARoute (User.Interface.intensityOfKeyPress 15))
    , test "KeyUp with a play note key" <|
        \_ ->
            let
                msg1 =
                    Message.KeyDown { key = KbdEvent.KeySpace }

                msg2 =
                    Message.AnimationFrame (Time.millisToPosix 15)

                msg3 =
                    Message.SelectChord Chord.C_Chord

                msg4 =
                    KeyUp { key = KbdEvent.KeyS }

                model2 =
                    { model | os = Tuple.first (OperatingSystem.update msg1 model.os selectors) }

                model3 =
                    { model2 | os = Tuple.first (OperatingSystem.update msg2 model2.os selectors) }
                model4 = case model3.instrument of
                  Just instrument ->
                    { model3 | instrument = Just (Tuple.first (Instrument.update msg3 instrument selectors)) }
                  Nothing ->
                    model3

                result =
                    contextualize msg4 model4
            in
            Expect.equal result (WithRouteVolumeVoiceIndexAndPitch NotARoute (User.Interface.intensityOfKeyPress 15) 0 55)
    , test "KeyDown with next item key on SelectChord route" <|
        \_ ->
            let
                msgKeyDown = Message.KeyDown { key = KbdEvent.KeyRight }
                router = model.router
                model2 = { model | router = { router | history = [SelectChordRoute] } }
                result = contextualize msgKeyDown model2
            in
            Expect.equal result (WithRouteAndSequenceItem SelectChordRoute Chord.A_Chord)
    ]
