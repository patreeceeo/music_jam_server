module OperatingSystem exposing (AppState(..), Model, init, milisSinceKeyDown, msgToCmd, update)

import Browser
import Browser.Events
import Browser.Navigation
import KbdEvent
import KbdState
import Message exposing (Message)
import PortMessage
import Selectors
import Time
import Url exposing (Url)
import Url.Parser


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
    , currentRoute : Maybe Routes
    , navKey : Browser.Navigation.Key
    }


init : Int -> Url -> Browser.Navigation.Key -> Model
init screenWidth url navKey =
    { appState = AppActive
    , timeInMillis = 0
    , kbdState = KbdState.init
    , screenWidth = screenWidth
    , currentRoute = urlToRoute url
    , navKey = navKey
    }



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
msgToCmd msg model =
    case msg of
        Message.VisibilityChange status ->
            PortMessage.send (PortMessage.AppStateChange (status == Browser.Events.Hidden))

        Message.UrlRequest req ->
            case req of
                Browser.Internal url ->
                    let
                        urlString =
                            Url.toString url
                    in
                    case urlToRoute url of
                        Just Faq ->
                            Browser.Navigation.load urlString

                        _ ->
                            Browser.Navigation.pushUrl model.navKey urlString

                Browser.External href ->
                    Browser.Navigation.load href

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


type Routes
    = Faq
    | Main


routeParser : Url.Parser.Parser (Routes -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Faq (Url.Parser.s "faq")
        , Url.Parser.map Main (Url.Parser.s "lab/fretboard")
        ]


urlToRoute : Url -> Maybe Routes
urlToRoute url =
    Url.Parser.parse routeParser url
