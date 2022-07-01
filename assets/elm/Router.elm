module Router exposing (Model, Routes(..), init, update)

import Browser
import Browser.Navigation
import Message exposing (Message)
import Selectors
import Url exposing (Url)
import Url.Parser exposing ((</>), map, oneOf, s)


type alias Model =
    { currentRoute : Maybe Routes
    , key : Browser.Navigation.Key
    }


type Routes
    = Faq
    | Main
    | SelectChord


routeParser : Url.Parser.Parser (Routes -> a) a
routeParser =
    oneOf
        [ map Faq (s "faq")
        , s "lab"
            </> oneOf
                    [ map Main (s "fretboard")
                    , map SelectChord (s "selectchord")
                    ]
        ]


urlToRoute : Url -> Maybe Routes
urlToRoute url =
    Url.Parser.parse routeParser url


init : Url -> Browser.Navigation.Key -> Model
init url key =
    { currentRoute = Debug.log "init route" (urlToRoute url)
    , key = key
    }


update : Message -> Model -> Selectors.Selectors -> ( Model, Cmd Message )
update msg model _ =
    case msg of
        Message.UrlRequest req ->
            case req of
                Browser.Internal url ->
                    let
                        urlString =
                            Url.toString url

                        newModel =
                            { model | currentRoute = urlToRoute url }
                    in
                    case urlToRoute url of
                        Just Faq ->
                            ( newModel, Browser.Navigation.load urlString )

                        _ ->
                            ( newModel
                            , Browser.Navigation.pushUrl model.key urlString
                            )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        Message.UrlChange url ->
            ( { model | currentRoute = urlToRoute url }, Cmd.none )

        Message.RequestPreviousUrl n ->
            -- will this trigger a UrlChange message? if not, need to update the model here
            ( model, Browser.Navigation.back model.key (Debug.log "going back x " n) )

        _ ->
            ( model, Cmd.none )
