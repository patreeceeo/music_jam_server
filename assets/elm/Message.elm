module Message exposing (Message(..))

import Browser.Events
import KbdEvent
import MouseEvent
import PortMessage
import Time
import CommonTypes exposing (Volume)


type Message
    = MouseOverVoice Int MouseEvent.Model
    | AnimationFrame Time.Posix
    | WindowResize Int
    | KeyDown KbdEvent.Model
    | KeyUp KbdEvent.Model
    | ReceivePortMessage PortMessage.RawMessage
    | VisibilityChange Browser.Events.Visibility
    | PlayChord Volume
