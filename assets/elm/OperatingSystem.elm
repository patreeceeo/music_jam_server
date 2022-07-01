module OperatingSystem exposing (AppState(..), Model, init, milisSinceKeyDown, msgToCmd, setError, update)

import Browser.Events
import KbdEvent
import KbdState
import Message exposing (Message)
import PortMessage
import Selectors
import Time


{-| Foundational bits and bytes that encapsulate and abstract the host system i.e. Web browser

@docs OperatingSystem

-}



-- TODO factor out a Router module?
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
    , errorMessage : String
    }


init : Int -> Model
init screenWidth =
    { appState = AppActive
    , timeInMillis = 0
    , kbdState = KbdState.init
    , screenWidth = screenWidth
    , errorMessage = "Something went wrong."
    }


setError : String -> Model -> Model
setError message model =
    { model | errorMessage = message }



-- UPDATE


update : Message -> Model -> Selectors.Selectors -> ( Model, Cmd Message )
update msg model _ =
    let
        cmd =
            msgToCmd msg model
    in
    case ( msg, model.appState ) of
        ( Message.VisibilityChange status, _ ) ->
            case status of
                Browser.Events.Hidden ->
                    ( { model | appState = AppSleeping }, cmd )

                Browser.Events.Visible ->
                    ( { model | appState = AppActive }, cmd )

        ( Message.AnimationFrame newTime, AppActive ) ->
            ( { model | timeInMillis = Time.posixToMillis newTime }, cmd )

        ( Message.WindowResize width, _ ) ->
            ( { model | screenWidth = width }, cmd )

        ( Message.KeyDown event, AppActive ) ->
            let
                newKeyState =
                    case KbdState.get event.key model.kbdState of
                        Just keyState ->
                            { keyState | lastPressedAt = model.timeInMillis }

                        Nothing ->
                            { lastPressedAt = model.timeInMillis }
            in
            ( { model | kbdState = KbdState.set event.key newKeyState model.kbdState }, cmd )

        _ ->
            ( model, cmd )


msgToCmd : Message -> Model -> Cmd Message
msgToCmd msg _ =
    case msg of
        Message.VisibilityChange status ->
            PortMessage.send (PortMessage.AppStateChange (status == Browser.Events.Hidden))

        _ ->
            Cmd.none



-- HELPERS


milisSinceKeyDown : KbdEvent.Key -> Model -> Int
milisSinceKeyDown key model =
    case KbdState.get key model.kbdState of
        Just keyState ->
            model.timeInMillis - keyState.lastPressedAt

        Nothing ->
            0
