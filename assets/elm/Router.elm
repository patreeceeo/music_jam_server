module Router exposing (Model, init, update)

import Browser
import Browser.Navigation
import Url exposing (Url)
import Url.Parser
import Message exposing (Message)
import Selectors

type alias Model =
    { currentRoute : Maybe Routes
    , key : Browser.Navigation.Key
    }

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


init : Url -> Browser.Navigation.Key -> Model
init url key =
  { currentRoute = urlToRoute url
  , key = key
  }

update : Message -> Model -> Selectors.Selectors -> (Model, Cmd Message)
update msg model _ =
    case msg of
        Message.UrlRequest req ->
            case req of
                Browser.Internal url ->
                    let
                        urlString =
                            Url.toString url
                        newModel = { model | currentRoute = urlToRoute url }
                    in
                    case urlToRoute url of
                        Just Faq ->
                            (newModel, Browser.Navigation.load urlString)

                        _ ->
                            ( newModel
                            , Browser.Navigation.pushUrl model.key urlString
                            )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        _ ->
            (model, Cmd.none)
