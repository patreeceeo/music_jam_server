module Message exposing (Message(..))

import Browser
import Browser.Events
import CommonTypes exposing (Volume)
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
