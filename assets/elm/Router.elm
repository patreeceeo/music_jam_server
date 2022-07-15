module Router exposing (Model, NavCmd(..), NavKey(..), currentRoute, init, update, update_)

import Browser
import Browser.Navigation
import CommonTypes exposing (Routes(..), Selectors)
import List.Extra
import Message exposing (Message)
import Url exposing (Url)
import Url.Parser exposing ((</>), map, oneOf, s)


type NavKey
    = ActualNavKey Browser.Navigation.Key
    | TestNavKey


type NavCmd
    = PushUrl NavKey String
    | GoBack NavKey Int
    | Load String
    | NoCmd


type alias Model =
    { key : NavKey
    , history : List Routes
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


urlToRoute : Url -> Routes
urlToRoute url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault NotARoute


init : Url -> Browser.Navigation.Key -> Model
init url key =
    { key = ActualNavKey key
    , history = [ urlToRoute url ]
    }


update_ : Message -> Model -> ( Model, NavCmd )
update_ msg model =
    case msg of
        Message.UrlRequest req ->
            case req of
                Browser.Internal url ->
                    let
                        urlString =
                            Url.toString url
                    in
                    case urlToRoute url of
                        FaqRoute ->
                            ( model, Load urlString )

                        _ ->
                            ( model
                            , PushUrl model.key urlString
                            )

                Browser.External href ->
                    ( model
                    , Load href
                    )

        Message.UrlChange url ->
            ( { model | history = List.append model.history [ urlToRoute url ] }, NoCmd )

        Message.RequestPreviousUrl n ->
            let
                newHistory =
                    pop n model.history

                cmd =
                    if List.length model.history >= n then
                        GoBack model.key n

                    else
                        NoCmd
            in
            ( { model | history = newHistory }, cmd )

        _ ->
            ( model, NoCmd )


update : Message -> Model -> Selectors -> ( Model, Cmd Message )
update msg model _ =
    let
        ( newModel, cmdWrapper ) =
            update_ msg model

        cmd =
            case cmdWrapper of
                PushUrl (ActualNavKey key) urlString ->
                    Browser.Navigation.pushUrl key urlString

                GoBack (ActualNavKey key) n ->
                    Browser.Navigation.back key n

                Load urlString ->
                    Browser.Navigation.load urlString

                _ ->
                    Cmd.none
    in
    ( newModel, cmd )


currentRoute : Model -> Routes
currentRoute model =
    List.Extra.last (Debug.log "history" model.history)
        |> Maybe.withDefault NotARoute


pop : Int -> List a -> List a
pop n list =
    List.take (List.length list - n) list
