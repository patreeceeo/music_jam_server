module Router exposing (Model, init, update)

import Browser
import Browser.Navigation
import Message exposing (Message)
import Url exposing (Url)
import Url.Parser exposing ((</>), map, oneOf, s)
import CommonTypes exposing (Routes(..), Selectors)


type alias Model =
    { currentRoute : Maybe Routes
    , key : Browser.Navigation.Key
    }

routeParser : Url.Parser.Parser (Routes -> a) a
routeParser =
    oneOf
        [ map FaqRoute (s "faq")
        , s "lab"
            </> oneOf
                    [ map MainRoute (s "fretboard")
                    , map SelectChordRoute (s "selectchord")
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


update : Message -> Model -> Selectors -> ( Model, Cmd Message )
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
                        Just FaqRoute ->
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
            ( model, Browser.Navigation.back model.key n )

        _ ->
            ( model, Cmd.none )


