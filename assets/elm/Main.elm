module Main exposing (Model, SubModels(..), main, update, view)

-- TODO remove

import Browser
import Browser.Events
import Browser.Navigation
import Chord
import CommonTypes exposing (ClientId, Routes(..), Selectors)
import Errors
import Html
import Html.Attributes
import Instrument
import Json.Decode as D
import KbdEvent
import Maybe.Extra
import Message exposing (Message)
import Modely
import OperatingSystem as OS
import PortMessage
import Router
import Url exposing (Url)
import User.Interface exposing (userActionForKey)
import User.Interface.Instrument exposing (viewSelectChord)
import UserInterfaces as UIs
import Utils



-- MAIN


main : Program D.Value Model Message
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Message.UrlRequest
        , onUrlChange = Message.UrlChange
        }



-- MODEL


type alias Flags =
    { clientId : ClientId
    , screenWidth : Int
    , baseHref : String
    , instrument : Instrument.Model
    }


init : D.Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd Message )
init flags url navKey =
    let
        router =
            Router.init url navKey
    in
    case D.decodeValue decodeFlags flags of
        Ok decodedFlags ->
            let
                instrument =
                    Just decodedFlags.instrument
            in
            ( { os = OS.init decodedFlags.clientId decodedFlags.screenWidth decodedFlags.baseHref
              , instrument = instrument
              , router = router
              , uiInstrument = User.Interface.Instrument.init
              }
            , Cmd.none
            )

        Err err ->
            let
                errMsg =
                    D.errorToString err

                errMsgWithContext =
                    Errors.instrumentDecoding errMsg
            in
            ( { os = OS.setError errMsgWithContext (OS.init "" 0 "")
              , instrument = Nothing
              , router = router
              , uiInstrument = User.Interface.Instrument.init
              }
            , PortMessage.send (PortMessage.LogError errMsgWithContext)
            )


decodeFlags : D.Decoder Flags
decodeFlags =
    D.map4 Flags
        (D.field "clientId" D.string)
        (D.field "screenWidth" D.int)
        (D.field "baseHref" D.string)
        (D.field "instrument" Instrument.decoder)


type alias Model =
    { os : OS.Model
    , instrument : Maybe Instrument.Model
    , router : Router.Model
    , uiInstrument : User.Interface.Instrument.Model
    }


type SubModels
    = OperatingSystemModel OS.Model
    | InstrumentModel (Maybe Instrument.Model)
    | UIInstrumentModel User.Interface.Instrument.Model
    | RouterModel Router.Model



-- UPDATE


composers : List (Modely.Composer Message Model SubModels Selectors ( SubModels, Cmd Message ))
composers =
    [ ( getOs
      , mapOs OS.update
      , setOs
      )
    , ( getInstrument
      , mapInstrument Instrument.update
      , setInstrument
      )
    , ( getRouter
      , mapRouter Router.update
      , setRouter
      )
    , ( getUIInstrument
      , mapUIInstrument User.Interface.Instrument.update
      , setUIInstrument
      )
    ]


{-| Create functions that can be used in subModel update functions to access other parts of the model
-}
bindSelectors : Model -> Selectors
bindSelectors model =
    { milisSinceKeyDown = \key -> OS.milisSinceKeyDown key model.os
    , timeInMillis = \() -> model.os.timeInMillis
    , screenWidth = \() -> model.os.screenWidth
    , currentRoute = \() -> model.router.currentRoute
    }


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    -- Translate route/message combinations into new messages for sub module updates
    let
        mappedMsgs =
            case ( model.router.currentRoute, msg ) of
                -- Ignore messages originating from this client
                ( _, Message.ReceivePortMessage clientId _ ) ->
                    if clientId == model.os.clientId then
                        []

                    else
                        [ msg ]

                ( Just MainRoute, Message.KeyDown event ) ->
                    case event.key of
                        KbdEvent.KeyEnter ->
                            case Url.fromString (model.os.baseHref ++ "/selectchord") of
                                Just url ->
                                    [ msg, Message.UrlRequest (Browser.Internal url) ]

                                Nothing ->
                                    [ msg ]

                        _ ->
                            [ msg ]

                ( Just MainRoute, Message.KeyUp event ) ->
                    let
                        milisSinceKeyDown =
                            OS.milisSinceKeyDown event.key model.os

                        volume =
                            User.Interface.intensityOfKeyPress milisSinceKeyDown
                    in
                    case event.key of
                        KbdEvent.KeySpace ->
                            [ msg, Message.PlayChord volume ]

                        _ ->
                            Maybe.map2
                                (\instrument voiceIndex ->
                                    case Instrument.currentPitch voiceIndex instrument of
                                        Just pitch ->
                                            [ msg, Message.PlayNote volume voiceIndex (Debug.log "pitch" pitch) ]

                                        Nothing ->
                                            [ msg ]
                                )
                                model.instrument
                                (User.Interface.voiceIndexForKey event.key)
                                |> Maybe.withDefault [ msg ]

                ( Just SelectChordRoute, Message.KeyDown event ) ->
                    let
                        userAction =
                            userActionForKey event.key
                    in
                    case userAction of
                        User.Interface.Dismiss ->
                            [ msg, Message.RequestPreviousUrl 1 ]

                        _ ->
                            let
                                newChordName =
                                    case userAction of
                                        User.Interface.SeekForward ->
                                            Chord.next model.uiInstrument.activeChordName

                                        User.Interface.SeekBackward ->
                                            Chord.previous model.uiInstrument.activeChordName

                                        _ ->
                                            model.uiInstrument.activeChordName
                            in
                            [ msg, Message.SelectChord newChordName ]

                _ ->
                    [ msg ]
    in
    -- Send original message as well as mapped message (if there is one)
    updateSubsX mappedMsgs model



-- TODO this is similar to code in Modely


updateSubsX : List Message -> Model -> ( Model, Cmd Message )
updateSubsX msgList model =
    let
        ( finalModel, cmdList ) =
            Utils.mapAccumr updateSubsFold ( model, [] ) msgList
    in
    ( finalModel, Cmd.batch cmdList )


updateSubsFold : ( Model, List (Cmd Message) ) -> Message -> ( Model, List (Cmd Message) )
updateSubsFold acc msg =
    let
        ( accModel, accCmd ) =
            acc

        ( newModel, newCmd ) =
            updateSubs msg accModel
    in
    ( newModel, List.append [ newCmd ] accCmd )


updateSubs : Message -> Model -> ( Model, Cmd Message )
updateSubs =
    Modely.compose composers Cmd.batch bindSelectors



-- UPDATE getters


getOs : Model -> SubModels
getOs =
    Utils.tagReturnWith OperatingSystemModel (\model -> model.os)


getInstrument : Model -> SubModels
getInstrument =
    Utils.tagReturnWith InstrumentModel (\model -> model.instrument)


getRouter : Model -> SubModels
getRouter =
    Utils.tagReturnWith RouterModel (\model -> model.router)


getUIInstrument : Model -> SubModels
getUIInstrument =
    Utils.tagReturnWith UIInstrumentModel (\model -> model.uiInstrument)



-- UPDATE setters


setOs : SubModels -> Model -> Model
setOs =
    Utils.untagP1 untagOs
        (\maybeOs model ->
            case maybeOs of
                Just os ->
                    { model | os = os }

                Nothing ->
                    model
        )


setInstrument : SubModels -> Model -> Model
setInstrument =
    Utils.untagP1 untagInstrument (\maybeInstrument model -> { model | instrument = maybeInstrument })


setRouter : SubModels -> Model -> Model
setRouter =
    Utils.untagP1 untagRouter
        (\maybeSubModel model ->
            case maybeSubModel of
                Just subModel ->
                    { model | router = subModel }

                Nothing ->
                    model
        )


setUIInstrument : SubModels -> Model -> Model
setUIInstrument =
    Utils.untagP1 untagUIInstrument
        (\maybeSubModel model ->
            case maybeSubModel of
                Just subModel ->
                    { model | uiInstrument = subModel }

                Nothing ->
                    model
        )



{- UPDATE untaggers
   Take a tagged subModel and attempt to return the subModel itself
   E.g.
   SubModel = CheeseSubModel Cheese | ...
   tagged = CheeseSubModel cheese
   maybeCheese = untagCheese tagged
-}


untagOs : SubModels -> Maybe OS.Model
untagOs tagged =
    case tagged of
        OperatingSystemModel os ->
            Just os

        _ ->
            Nothing


untagInstrument : SubModels -> Maybe Instrument.Model
untagInstrument tagged =
    case tagged of
        InstrumentModel ins ->
            ins

        _ ->
            Nothing


untagRouter : SubModels -> Maybe Router.Model
untagRouter tagged =
    case tagged of
        RouterModel model ->
            Just model

        _ ->
            Nothing


untagUIInstrument : SubModels -> Maybe User.Interface.Instrument.Model
untagUIInstrument tagged =
    case tagged of
        UIInstrumentModel model ->
            Just model

        _ ->
            Nothing



{- UPDATE mappers

   Take the update function for a submodel and return a function that will attempt to untag its model argument and tag the model in the return value.
   E.g.
   SubModel = CheeseSubModel Cheese | ...
   updateCheese = Cheese -> Msg -> (Cheese, Cmd Msg)
   updateCheeseMapped = SubModel -> Msg -> (SubModel, Cmd Msg)

-}


mapOs : (Message -> OS.Model -> Selectors -> ( OS.Model, Cmd Message )) -> Message -> SubModels -> Selectors -> ( SubModels, Cmd Message )
mapOs update_ msg taggedModel selectors =
    case untagOs taggedModel of
        Just os ->
            Tuple.mapFirst OperatingSystemModel (update_ msg os selectors)

        Nothing ->
            ( Debug.log "received unexpected type " taggedModel, Cmd.none )


mapInstrument : (Message -> Instrument.Model -> Selectors -> ( Instrument.Model, Cmd Message )) -> Message -> SubModels -> Selectors -> ( SubModels, Cmd Message )
mapInstrument update_ msg taggedModel selectors =
    case untagInstrument taggedModel of
        Just instrument ->
            Tuple.mapFirst (\m -> InstrumentModel (Just m)) (update_ msg instrument selectors)

        Nothing ->
            ( Debug.log "received unexpected type " taggedModel, Cmd.none )


mapRouter : (Message -> Router.Model -> Selectors -> ( Router.Model, Cmd Message )) -> Message -> SubModels -> Selectors -> ( SubModels, Cmd Message )
mapRouter update_ msg taggedModel selectors =
    case untagRouter taggedModel of
        Just model ->
            Tuple.mapFirst RouterModel (update_ msg model selectors)

        Nothing ->
            ( Debug.log "received unexpected type " taggedModel, Cmd.none )


mapUIInstrument : (Message -> User.Interface.Instrument.Model -> Selectors -> ( User.Interface.Instrument.Model, Cmd Message )) -> Message -> SubModels -> Selectors -> ( SubModels, Cmd Message )
mapUIInstrument update_ msg taggedModel selectors =
    case untagUIInstrument taggedModel of
        Just model ->
            Tuple.mapFirst UIInstrumentModel (update_ msg model selectors)

        Nothing ->
            ( Debug.log "received unexpected type " taggedModel, Cmd.none )



-- SUBS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrame Message.AnimationFrame
        , Browser.Events.onResize (\w _ -> Message.WindowResize w)
        , Browser.Events.onVisibilityChange Message.VisibilityChange
        , Browser.Events.onKeyDown (KbdEvent.decode |> D.map Message.KeyDown)
        , Browser.Events.onKeyUp (KbdEvent.decode |> D.map Message.KeyUp)
        , PortMessage.receive (Message.ReceivePortMessage model.os.clientId)
        ]



-- VIEW


view : Model -> Browser.Document Message.Message
view model =
    { title = "Loopy Fruits MOIP"
    , body = body model
    }


body : Model -> List (Html.Html Message.Message)
body model =
    let
        maybeCurrentRoute =
            model.router.currentRoute

        fallbackHtml =
            Html.p [] [ Html.text model.os.errorMessage ]
    in
    [ UIs.navMenu
    , Maybe.Extra.unwrap fallbackHtml
        (\currentRoute ->
            if List.member currentRoute [ MainRoute, SelectChordRoute ] then
                Maybe.Extra.unwrap fallbackHtml
                    (\instrument ->
                        let
                            instrumentHtml =
                                UIs.instrument instrument model.os Message.MouseOverVoice

                            selectChordLink =
                                Html.a [ Html.Attributes.href "/lab/selectchord" ] [ Html.text "[change]" ]

                            currentChord =
                                Html.span [] [ Html.text ("chord: " ++ Chord.nameToStr model.uiInstrument.activeChordName) ]

                            defaultHtml =
                                [ instrumentHtml, currentChord, selectChordLink ]
                        in
                        Html.div []
                            (if currentRoute == SelectChordRoute then
                                defaultHtml ++ [ viewSelectChord model.uiInstrument ]

                             else
                                defaultHtml
                            )
                    )
                    model.instrument

            else
                fallbackHtml
        )
        maybeCurrentRoute
    ]
