module InterpretTests exposing (suite)

import Browser
import CommonTypes exposing (Routes(..))
import Expect
import Json.Encode as E
import KbdEvent
import Message exposing (Message(..))
import Main exposing (Context(..), interpret)
import Test exposing (..)
import Url
import User.Interface exposing (UserAction(..))
import Chord exposing (Names(..))


suite : Test
suite = describe "interpret" interpretTests

interpretTests : List Test
interpretTests =
    [ test "omits ReceivePortMessage on any route when sent by self" <|
        \_ ->
            let
                msg =
                    Message.ReceivePortMessage "bach" (E.int 0)

                result =
                    interpret msg (WithClientId "bach")
            in
            Expect.equal [] result
    , test "passes thru ReceivePortMessage on any route when sent by other" <|
        \_ ->
            let
                msg =
                    Message.ReceivePortMessage "williams" (E.int 0)

                result =
                    interpret msg (WithClientId "waititi")
            in
            Expect.equal [ msg ] result
    , test "KeyDown with enter key on any route requests next route url" <|
        \_ ->
            let
                msgIn =
                    Message.KeyDown { key = KbdEvent.KeyEnter }

                maybeUrl =
                    Url.fromString "https://example.com"

                msgs =
                    interpret msgIn (WithMaybeUrl maybeUrl)
            in
            case maybeUrl of
                Just url ->
                    Expect.equal msgs [ msgIn, Message.UrlRequest (Browser.Internal url) ]

                Nothing ->
                    Expect.fail "example url sux"
    , test "KeyUp with play chord key plays a chord on MainRoute" <|
        \_ ->
            let
                msgIn =
                    Message.KeyUp { key = KbdEvent.KeySpace }

                msgs =
                    interpret msgIn (WithRouteAndVolume MainRoute 0.5)
            in
            Expect.equal msgs [ msgIn, PlayChord 0.5 ]
    , test "KeyUp with a play note key plays a note on MainRoute" <|
        \_ ->
            let
                msgIn =
                    Message.KeyUp { key = KbdEvent.KeyS }

                msgs =
                    interpret msgIn (WithRouteVolumeVoiceIndexAndPitch MainRoute 0.5 4 42)
            in
            Expect.equal msgs [ msgIn, PlayNote 0.5 4 42 ]
    , test "KeyDown with next item on SelectChord route selects next chord" <|
        \_ ->
          let
              msgIn =
                Message.KeyDown { key = KbdEvent.KeyA } -- Doesn't matter what key
              msgs =
                  interpret msgIn (WithRouteAndSequenceItem SelectChordRoute A_Chord)
          in
          Expect.equal msgs [ msgIn, SelectChord A_Chord ]
    ]
