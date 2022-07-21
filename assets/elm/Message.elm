module Message exposing (Context(..), Message(..), interpret)

import Browser
import Browser.Events
import Chord
import CommonTypes exposing (ClientId, Inputs, NoteVIndex, Pitch, Routes(..), Volume)
import KbdEvent
import MouseEvent
import PortMessage
import Time
import Url exposing (Url)


type Message
    = MouseOverVoice Int MouseEvent.Model
    | AnimationFrame Time.Posix
    | WindowResize Int
    | KeyDown KbdEvent.Model
    | KeyUp KbdEvent.Model
    | ReceivePortMessage ClientId PortMessage.RawMessage
    | VisibilityChange Browser.Events.Visibility
    | PlayNote Volume NoteVIndex Pitch
    | PlayChord Volume
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    | SelectChord Chord.Names
    | SetUIActiveChord Chord.Names
    | RequestPreviousUrl Int
    | FocusChange Inputs
    | Nevermind


type Context
    = WithRoute Routes
    | WithMaybeUrl (Maybe Url)
    | WithClientId ClientId
    | WithRouteAndVolume Routes Volume
    | WithRouteVolumeNoteVIndexAndPitch Routes Volume NoteVIndex Pitch
    | WithRouteAndSequenceItem Routes Chord.Names



-- TODO(optimize): find way to reduce the number of parameters. One way would be including the necessary info in the corresponding variants of an abstract type that wraps Message, then use that instead of Message.


interpret : Message -> Context -> List Message
interpret msg context =
    case ( msg, context ) of
        ( ReceivePortMessage sender payload, WithClientId clientId ) ->
            -- Ignore messages sent to self
            if sender == clientId then
                []

            else
                [ ReceivePortMessage sender payload ]

        ( KeyDown event, WithMaybeUrl maybeUrl ) ->
            case event.key of
                KbdEvent.KeyEnter ->
                    maybeUrl
                        |> Maybe.map (\url -> [ msg, UrlRequest (Browser.Internal url) ])
                        |> Maybe.withDefault [ msg ]

                _ ->
                    [ msg ]

        ( KeyUp { key }, WithRouteAndVolume MainRoute intensityOfPress ) ->
            case key of
                KbdEvent.KeySpace ->
                    [ msg, PlayChord intensityOfPress ]

                _ ->
                    [ msg ]

        ( KeyUp _, WithRouteVolumeNoteVIndexAndPitch MainRoute volume noteVIndex intensityOfPress ) ->
            [ msg, PlayNote volume noteVIndex intensityOfPress ]

        ( KeyDown _, WithRouteAndSequenceItem SelectChordRoute chord ) ->
            [ msg, SelectChord chord ]

        _ ->
            [ msg ]



--      ( SelectChordRoute, Message.KeyDown event ) ->
--          let
--              userAction =
--                  userActionForKey event.key
--          in
--          case userAction of
--              User.Interface.Dismiss ->
--                  [ msg, Message.RequestPreviousUrl 1 ]
--              _ ->
--                  let
--                      newChordName =
--                          case userAction of
--                              User.Interface.SeekForward ->
--                                  Chord.next model.uiInstrument.activeChordName
--                              User.Interface.SeekBackward ->
--                                  Chord.previous model.uiInstrument.activeChordName
--                              _ ->
--                                  model.uiInstrument.activeChordName
--                  in
--                  [ msg, Message.SelectChord newChordName ]
--      _ ->
--          [ msg ]
