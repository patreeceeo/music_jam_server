module OperatingSystem exposing (AppState(..), Model, Msg(..), handleVisibilityChange, init, subscriptions, update)

import Browser.Events
import Json.Decode as D
import KbdEvent
import KbdState
import PortMessage
import Time


{-| Foundational bits and bytes that encapsulate and abstract the host system i.e. Web browser

@docs OperatingSystem

-}



-- TODO use Float unless it really has to be an Int? Or vice versa?
-- MODEL


type AppState
    = AppSleeping
    | AppActive


type alias Model =
    { appState : AppState
    , timeInMillis : Int
    , screenWidth : Int
    , kbdState : KbdState.Model
    }


init : Int -> Model
init screenWidth =
    { appState = AppActive
    , timeInMillis = 0
    , kbdState = KbdState.init
    , screenWidth = screenWidth
    }



-- UPDATE


type Msg
    = AnimationFrame Time.Posix
    | WindowResize Int
    | KeyDown KbdEvent.Model
    | KeyUp KbdEvent.Model
    | ReceivePortMessage PortMessage.RawMessage
    | VisibilityChange Browser.Events.Visibility


handleVisibilityChange : Browser.Events.Visibility -> Model -> ( Model, Cmd a )
handleVisibilityChange status model =
    case status of
        Browser.Events.Hidden ->
            ( { model | appState = AppSleeping }
            , PortMessage.send (PortMessage.AppStateChange True)
            )

        Browser.Events.Visible ->
            ( { model | appState = AppActive }
            , PortMessage.send (PortMessage.AppStateChange False)
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.appState of
        AppSleeping ->
            case msg of
                VisibilityChange status ->
                    handleVisibilityChange status model

                _ ->
                    ( model, Cmd.none )

        AppActive ->
            case msg of
                AnimationFrame newTime ->
                    ( { model
                        | timeInMillis = Time.posixToMillis newTime
                      }
                    , Cmd.none
                    )

                WindowResize width ->
                    ( { model | screenWidth = width }
                    , Cmd.none
                    )

                KeyDown event ->
                    let
                        newKeyState =
                            case KbdState.get event.key model.kbdState of
                                Just keyState ->
                                    { keyState | lastPressedAt = model.timeInMillis }

                                Nothing ->
                                    { lastPressedAt = model.timeInMillis }
                    in
                    ( { model | kbdState = KbdState.set event.key newKeyState model.kbdState }
                    , Cmd.none
                    )

                KeyUp _ ->
                    ( model, Cmd.none )

                ReceivePortMessage _ ->
                    ( model, Cmd.none )

                VisibilityChange status ->
                    handleVisibilityChange status model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame AnimationFrame
        , Browser.Events.onResize (\w _ -> WindowResize w)
        , Browser.Events.onVisibilityChange VisibilityChange
        , Browser.Events.onKeyDown (KbdEvent.decode |> D.map KeyDown)
        , Browser.Events.onKeyUp (KbdEvent.decode |> D.map KeyUp)
        , PortMessage.receive ReceivePortMessage
        ]
