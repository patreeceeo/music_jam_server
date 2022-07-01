module Message exposing (Message(..))

import Browser
import Browser.Events
import Chord
import CommonTypes exposing (Inputs, Volume)
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
    | ReceivePortMessage PortMessage.RawMessage
    | VisibilityChange Browser.Events.Visibility
    | PlayChord Volume
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    | SelectChord Chord.Names
    | SetUIActiveChord Chord.Names
    | RequestPreviousUrl Int
    | FocusChange Inputs
    | Nevermind
