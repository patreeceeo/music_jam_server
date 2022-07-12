module Message exposing (Message(..))

import Browser
import Browser.Events
import Chord
import CommonTypes exposing (ClientId, Inputs, NoteVIndex, Pitch, Volume)
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
