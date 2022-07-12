module KbdState exposing (Model, get, init, set, isPressed, setPressed, KeyPressedState(..))

import Dict exposing (Dict)
import KbdEvent exposing (Key(..))
import List.Extra
import CommonTypes exposing (QTime)

type KeyPressedState = KeyIsPressed | KeyIsReleased

type alias KeyState =
    { lastPressedAt : Int
    , pressedState: KeyPressedState
    }

initKeyState = { lastPressedAt = 0
               , pressedState = KeyIsReleased
               }

type alias Model =
    Dict Int KeyState


keyOrder : List Key
keyOrder =
    [ KeyA
    , KeyS
    , KeyD
    , KeyF
    , KeyG
    , KeyH
    , KeyJ
    , KeyK
    , KeyL
    , KeySpace
    , KeyLeft
    , KeyRight
    , KeyUp
    , KeyDown
    ]


keyToInt : Key -> Maybe Int
keyToInt needle =
    List.Extra.findIndex (\key -> key == needle) keyOrder


set : Key -> KeyState -> Model -> Model
set key state model =
    case keyToInt key of
        Just intKey ->
            Dict.insert intKey state model

        Nothing ->
            model


get : Key -> Model -> Maybe KeyState
get key model =
    case keyToInt key of
        Just intKey ->
            Dict.get intKey model

        _ ->
            Nothing

isPressed : Key -> Model -> Bool
isPressed key model =
  case get key model of
    Just keyState ->
      keyState.pressedState == KeyIsPressed
    Nothing ->
      False

setPressed : Key -> Bool -> QTime -> Model -> Model
setPressed key bool time model =
  let
      maybeKeyState = get key model
      newPressedState = if bool then
          KeyIsPressed
        else
          KeyIsReleased
  in
  case maybeKeyState of
    Just currentKeyState ->
      if newPressedState == KeyIsPressed then
        set key { currentKeyState | pressedState = newPressedState, lastPressedAt = time } model
      else
        set key { currentKeyState | pressedState = newPressedState } model
    Nothing ->
      model


init : Model
init =
    Dict.fromList []
