module KbdState exposing (Model, set, get, init)
import KbdEvent exposing (Key(..))
import List.Extra
import Dict exposing (Dict)

type alias KeyState =
  { lastPressedAt: Int
  }

type alias Model = Dict Int KeyState
keyOrder : List Key
keyOrder = [ KeyA
  , KeyS
  , KeyD
  , KeyF
  , KeyG
  , KeyH
  , KeyJ
  , KeyK
  , KeyL
  ]

keyToInt : Key -> Maybe Int
keyToInt needle =
  List.Extra.findIndex (\key -> key == needle) keyOrder

set : Key -> KeyState -> Model -> Model
set key state model =
  case (keyToInt key) of
    Just intKey ->
      Dict.insert intKey state model
    Nothing ->
      model

get : Key -> Model -> Maybe KeyState
get key model =
  case (keyToInt key) of
    Just intKey ->
      Dict.get intKey model
    Nothing ->
      Nothing

init : Model
init =
  Dict.fromList []

