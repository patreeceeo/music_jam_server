module Router exposing (Model, init, update)

import Browser
import Browser.Navigation
import CommonTypes exposing (Routes(..), Selectors)
import List.Extra
import Message exposing (Message)
import Url exposing (Url)
import Url.Parser exposing ((</>), map, oneOf, s)
import Utils


type alias Model =
    { currentRoute : Maybe Routes
    , key : Browser.Navigation.Key
    , history : List Url
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
    , history = [ url ]
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
                    in
                    case urlToRoute url of
                        Just FaqRoute ->
                            ( model, Browser.Navigation.load urlString )

                        _ ->
                            ( { model | history = List.append model.history [ url ], currentRoute = urlToRoute url }
                            , Browser.Navigation.pushUrl model.key urlString
                            )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        Message.UrlChange url ->
            ( { model | currentRoute = urlToRoute url }, Cmd.none )

        Message.RequestPreviousUrl n ->
            let
                newHistory =
                    pop n model.history
            in
            case List.Extra.last newHistory of
                Just previousUrl ->
                    -- will this trigger a UrlChange message? if not, need to update the model here
                    ( { model | currentRoute = urlToRoute previousUrl, history = newHistory }, Browser.Navigation.back model.key n )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


pop : Int -> List a -> List a
pop n list =
    List.take (List.length list - n) list
