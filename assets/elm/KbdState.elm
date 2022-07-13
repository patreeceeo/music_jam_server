module KbdState exposing (KeyPressedState(..), Model, get, init, isPressed, set, setPressed)

import CommonTypes exposing (QTime)
import Dict exposing (Dict)
import KbdEvent exposing (Key(..))
import List.Extra


type KeyPressedState
    = KeyIsPressed
    | KeyIsReleased


type alias KeyState =
    { lastPressedAt : Int
    , pressedState : KeyPressedState
    }


initKeyState : KeyState
initKeyState =
    { lastPressedAt = 0
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
    , KeyEsc
    , KeyEnter
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
    if bool then
        let
            currentKeyState =
                get key model
                    |> Maybe.withDefault initKeyState

            newKeyState =
                { currentKeyState | pressedState = KeyIsPressed, lastPressedAt = time }
        in
        set key newKeyState model

    else
        case get key model of
            Just currentKeyState ->
                set key { currentKeyState | pressedState = KeyIsReleased } model

            Nothing ->
                model


init : Model
init =
    Dict.fromList []
