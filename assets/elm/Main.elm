module Main exposing (Model, SubModels(..), main, update, view)

import Browser
import Browser.Events
import Html
import Instrument
import Json.Decode as D
import KbdEvent
import Message exposing (Message)
import Modely
import OperatingSystem as OS
import PortMessage
import Selectors
import Svg.Attributes exposing (..)
import UserInterfaces as UIs
import Utils



-- MAIN


main : Program D.Value Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Flags =
    { screenWidth : Int
    , instrument : Instrument.Model
    }


init : D.Value -> ( Model, Cmd Message )
init flags =
    case D.decodeValue decodeFlags flags of
        Ok decodedFlags ->
            ( { os = OS.init decodedFlags.screenWidth
              , instrument = Just (.instrument decodedFlags)
              }
            , Cmd.none
            )

        Err errMsg ->
            ( { os = OS.init 0
              , instrument = Nothing
              }
            , PortMessage.send (PortMessage.LogError (D.errorToString errMsg))
            )


decodeFlags : D.Decoder Flags
decodeFlags =
    D.map2 Flags
        (D.field "screenWidth" D.int)
        (D.field "instrument" Instrument.decoder)


type alias Model =
    { os : OS.Model
    , instrument : Maybe Instrument.Model
    }


type SubModels
    = OperatingSystemModel OS.Model
    | InstrumentModel (Maybe Instrument.Model)



-- UPDATE


composers : List (Modely.Composer Message Model SubModels Selectors.Selectors ( SubModels, Cmd Message ))
composers =
    [ ( getOs
      , mapOs OS.update
      , setOs
      )
    , ( getInstrument
      , mapInstrument Instrument.update
      , setInstrument
      )
    ]


{-| Create functions that can be used in subModel update functions to access other parts of the model
-}
bindSelectors : Model -> Selectors.Selectors
bindSelectors model =
    { milisSinceKeyDown = \key -> OS.milisSinceKeyDown key model.os
    , timeInMillis = \() -> model.os.timeInMillis
    , screenWidth = \() -> model.os.screenWidth
    }


update : Message -> Model -> ( Model, Cmd Message )
update =
    Modely.compose composers Cmd.batch bindSelectors



-- UPDATE getters


getOs : Model -> SubModels
getOs =
    Utils.tagReturnWith OperatingSystemModel (\model -> model.os)


getInstrument : Model -> SubModels
getInstrument =
    Utils.tagReturnWith InstrumentModel (\model -> model.instrument)



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



{- UPDATE mappers

   Take the update function for a submodel and return a function that will attempt to untag its model argument and tag the model in the return value.
   E.g.
   SubModel = CheeseSubModel Cheese | ...
   updateCheese = Cheese -> Msg -> (Cheese, Cmd Msg)
   updateCheeseMapped = SubModel -> Msg -> (SubModel, Cmd Msg)

-}


mapOs : (Message -> OS.Model -> Selectors.Selectors -> ( OS.Model, Cmd Message )) -> Message -> SubModels -> Selectors.Selectors -> ( SubModels, Cmd Message )
mapOs update_ msg taggedModel selectors =
    case untagOs taggedModel of
        Just os ->
            Tuple.mapFirst OperatingSystemModel (update_ msg os selectors)

        Nothing ->
            ( Debug.log "received unexpected type " taggedModel, Cmd.none )


mapInstrument : (Message -> Instrument.Model -> Selectors.Selectors -> ( Instrument.Model, Cmd Message )) -> Message -> SubModels -> Selectors.Selectors -> ( SubModels, Cmd Message )
mapInstrument update_ msg taggedModel selectors =
    case untagInstrument taggedModel of
        Just instrument ->
            Tuple.mapFirst (\m -> InstrumentModel (Just m)) (update_ msg instrument selectors)

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
        , PortMessage.receive Message.ReceivePortMessage
        ]



-- VIEW


view : Model -> Html.Html Message
view model =
    case model.instrument of
        Just instrument ->
            Html.div []
                [ UIs.instrument instrument model.os Message.MouseOverVoice
                ]

        Nothing ->
            Html.p [] [ Html.text "Uh oh there was an error! Looks like the programmers goofed up the JSON encoding/decoding" ]
