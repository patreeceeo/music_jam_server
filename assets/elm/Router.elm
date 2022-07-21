module Router exposing (Model, NavCmd(..), currentRoute, init, routeToUrl, update, update_)

import Browser
import Browser.Navigation
import CommonTypes exposing (Routes(..), Selectors)
import List.Extra
import Message exposing (Message)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), map, oneOf, s)
import Utils exposing (TestableNavKey(..))


type NavCmd
    = PushUrl TestableNavKey String
    | GoBack TestableNavKey Int
    | Load String
    | NoCmd


type alias Model =
    { key : TestableNavKey
    , history : List Routes
    , baseHref : String
    , nextRoute : Routes
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


routeToUrl : Routes -> String
routeToUrl route =
    case route of
        MainRoute ->
            Url.Builder.absolute [ "lab", "fretboard" ] []

        SelectChordRoute ->
            Url.Builder.absolute [ "lab", "selectchord" ] []

        FaqRoute ->
            Url.Builder.absolute [ "faq" ] []

        NotARoute ->
            "/404"


init : Maybe Url -> TestableNavKey -> String -> Routes -> Model
init maybeUrl key baseHref nextRoute =
    { key = key
    , history =
        case maybeUrl of
            Just url ->
                [ urlToRoute url ]

            Nothing ->
                []
    , baseHref = baseHref
    , nextRoute = nextRoute
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



-- TODO(perf): push current route onto beginning of list


currentRoute : Model -> Routes
currentRoute model =
    List.Extra.last model.history
        |> Maybe.withDefault NotARoute


pop : Int -> List a -> List a
pop n list =
    List.take (List.length list - n) list
