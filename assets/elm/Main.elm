module Main exposing (Context(..), Model, SubModels(..), bindSelectors, contextualize, init, interpret, main, update, view)

-- TODO remove

import Browser
import Browser.Events
import Browser.Navigation
import Chord
import CommonTypes exposing (ClientId, Pitch, Routes(..), Selectors, Volume)
import Errors
import Flags
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
        { init = Flags.tryDecodingFor (wrapInit init)
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Message.UrlRequest
        , onUrlChange = Message.UrlChange
        }



-- MODEL


wrapInit : (firstArg -> Maybe Url -> Utils.TestableNavKey -> ( Model, Cmd Message )) -> firstArg -> Url -> Browser.Navigation.Key -> ( Model, Cmd Message )
wrapInit fn firstArg url navKey =
    fn firstArg (Just url) (Utils.ActualNavKey navKey)


init : Result D.Error Flags.Model -> Maybe Url -> Utils.TestableNavKey -> ( Model, Cmd Message )
init decodeFlagsResult url navKey =
    case decodeFlagsResult of
        Ok flags ->
            let
                instrument =
                    Just flags.instrument
            in
            ( { os = OS.init flags.clientId flags.screenWidth

              -- TODO pass voices to Instrument.init to create the instrument model
              , instrument = instrument
              , router = Router.init url navKey flags.baseHref flags.nextRoute
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
            ( { os = OS.setError errMsgWithContext (OS.init "" 0)
              , instrument = Nothing
              , router = Router.init Nothing Utils.TestNavKey "" NotARoute
              , uiInstrument = User.Interface.Instrument.init
              }
            , PortMessage.send (PortMessage.LogError errMsgWithContext)
            )


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


type Context
    = WithRoute Routes
    | WithMaybeUrl (Maybe Url)
    | WithClientId ClientId
    | WithRouteAndVolume Routes Volume
    | WithRouteVolumeVoiceIndexAndPitch Routes Volume Int Pitch
    | WithRouteAndSequenceItem Routes Chord.Names
    | WithNothing


contextualize : Message -> Model -> Context
contextualize msg model =
    case msg of
        Message.ReceivePortMessage _ _ ->
            WithClientId model.os.clientId

        Message.KeyDown event ->
            let
                userActionRequested =
                    userActionForKey event.key

                currentRoute =
                    Router.currentRoute model.router

                currentChordName =
                    model.uiInstrument.activeChordName
            in
            case ( currentRoute, userActionRequested ) of
                ( _, User.Interface.Open ) ->
                    WithRoute model.router.nextRoute

                ( SelectChordRoute, User.Interface.SeekForward ) ->
                    WithRouteAndSequenceItem currentRoute (Chord.next currentChordName)

                ( SelectChordRoute, User.Interface.SeekBackward ) ->
                    WithRouteAndSequenceItem currentRoute (Chord.previous currentChordName)

                _ ->
                    WithNothing

        Message.KeyUp event ->
            let
                currentRoute =
                    Router.currentRoute model.router

                millisSinceKeyDown =
                    OS.milisSinceKeyDown KbdEvent.KeySpace model.os

                volume =
                    User.Interface.intensityOfKeyPress millisSinceKeyDown
            in
            case event.key of
                KbdEvent.KeySpace ->
                    WithRouteAndVolume currentRoute volume

                _ ->
                    case ( model.instrument, User.Interface.voiceIndexForKey event.key ) of
                        ( Just instrument, Just voiceIndex ) ->
                            let
                                pitchMaybe =
                                    Instrument.currentPitch voiceIndex instrument
                            in
                            case pitchMaybe of
                                Just pitch ->
                                    WithRouteVolumeVoiceIndexAndPitch currentRoute volume voiceIndex pitch

                                _ ->
                                    WithNothing

                        _ ->
                            WithNothing

        _ ->
            WithNothing



-- TODO(optimize): find way to reduce the number of parameters. One way would be including the necessary info in the corresponding variants of an abstract type that wraps Message, then use that instead of Message.


interpret : Message -> Context -> List Message
interpret msg context =
    case ( msg, context ) of
        ( Message.ReceivePortMessage sender payload, WithClientId clientId ) ->
            -- Ignore messages sent to self
            if sender == clientId then
                []

            else
                [ Message.ReceivePortMessage sender payload ]

        ( Message.KeyDown event, WithMaybeUrl maybeUrl ) ->
            case event.key of
                KbdEvent.KeyEnter ->
                    maybeUrl
                        |> Maybe.map (\url -> [ msg, Message.UrlRequest (Browser.Internal url) ])
                        |> Maybe.withDefault [ msg ]

                _ ->
                    [ msg ]

        ( Message.KeyUp { key }, WithRouteAndVolume MainRoute intensityOfPress ) ->
            case key of
                KbdEvent.KeySpace ->
                    [ msg, Message.PlayChord intensityOfPress ]

                _ ->
                    [ msg ]

        ( Message.KeyUp _, WithRouteVolumeVoiceIndexAndPitch MainRoute volume voiceIndex intensityOfPress ) ->
            [ msg, Message.PlayNote volume voiceIndex intensityOfPress ]

        ( Message.KeyDown _, WithRouteAndSequenceItem SelectChordRoute chord ) ->
            [ msg, Message.SelectChord chord ]

        _ ->
            [ msg ]


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
    , currentRoute = \() -> Router.currentRoute model.router
    }


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    -- Translate route/message combinations into new messages for sub module updates
    let
        mappedMsgs =
            interpret msg (contextualize msg model)
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
        -- [ Browser.Events.onAnimationFrame Message.AnimationFrame
        [ Browser.Events.onResize (\w _ -> Message.WindowResize w)
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
        currentRoute =
            Router.currentRoute model.router

        fallbackHtml =
            Html.p [] [ Html.text model.os.errorMessage ]
    in
    [ UIs.navMenu
    , if List.member currentRoute [ MainRoute, SelectChordRoute ] then
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
    ]
