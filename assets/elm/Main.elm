module Main exposing (Model, Msg(..), main, update, view)

import Array
import Browser
import Html
import Instrument
import Json.Decode as D
import KbdEvent
import MouseEvent
import OperatingSystem as OS
import PortMessage
import Svg.Attributes exposing (..)
import UserInterfaces as UIs



-- CONSTANTS


mouseOverVoiceVolume : Float
mouseOverVoiceVolume =
    0.5



-- MAIN


main : Program D.Value Model Msg
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


type alias Model =
    { os : OS.Model
    , instrument : Maybe Instrument.Model
    }


init : D.Value -> ( Model, Cmd Msg )
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



-- UPDATE


type Msg
    = OSMsg OS.Msg
    | MouseOverVoice Int MouseEvent.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.instrument ) of
        ( OSMsg (OS.KeyUp event), Just instrument ) ->
            case voiceIndexForKey event.key of
                Just voiceIndex ->
                    let
                        volume =
                            intensityOfKeyPress model event.key
                    in
                    case Array.get voiceIndex instrument.voices of
                        Just voice ->
                            case Array.get 0 voice.notes of
                                Just pitch ->
                                    ( { model | instrument = Just (Instrument.playNote instrument voiceIndex pitch volume model.os.timeInMillis) }
                                    , PortMessage.send (PortMessage.PlaySound { soundId = "acoustic-guitar", voiceIndex = voiceIndex, pitch = pitch, volume = volume })
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( MouseOverVoice index event, Just instrument ) ->
            if event.buttons > 0 then
                let
                    pitchResult =
                        Instrument.pitchAtOffset event.offsetX model.os.screenWidth instrument index
                in
                case pitchResult of
                    Ok pitch ->
                        ( { model | instrument = Just (Instrument.playNote instrument index pitch mouseOverVoiceVolume model.os.timeInMillis) }
                        , PortMessage.send (PortMessage.PlaySound { soundId = "acoustic-guitar", voiceIndex = index, pitch = pitch, volume = mouseOverVoiceVolume })
                        )

                    Err errMsg ->
                        ( model, PortMessage.send (PortMessage.LogError errMsg) )

            else
                ( model, Cmd.none )

        ( OSMsg (OS.ReceivePortMessage rawMsg), Just instrument ) ->
            case PortMessage.decode rawMsg of
                Ok playSound ->
                    ( { model | instrument = Just (Instrument.playNote instrument playSound.data.voiceIndex playSound.data.pitch playSound.data.volume model.os.timeInMillis) }
                    , Cmd.none
                    )

                Err error ->
                    ( model
                    , PortMessage.send (PortMessage.LogError (D.errorToString error))
                    )

        ( OSMsg subMsg, _ ) ->
            OS.update subMsg model.os
                |> updateWith (\os -> { model | os = os }) OSMsg

        ( _, Nothing ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map OSMsg (OS.subscriptions model.os)



-- VIEW


view : Model -> Html.Html Msg
view model =
    case model.instrument of
        Just instrument ->
            Html.div []
                [ UIs.instrument instrument model.os MouseOverVoice
                ]

        Nothing ->
            Html.p [] [ Html.text "Uh oh there was an error! Looks like the programmers goofed up the JSON encoding/decoding" ]


voiceIndexForKey : KbdEvent.Key -> Maybe Int
voiceIndexForKey key =
    case key of
        KbdEvent.KeyS ->
            Just 0

        KbdEvent.KeyD ->
            Just 1

        KbdEvent.KeyF ->
            Just 2

        KbdEvent.KeyJ ->
            Just 3

        KbdEvent.KeyK ->
            Just 4

        KbdEvent.KeyL ->
            Just 5

        _ ->
            Nothing


intensityOfKeyPress : Model -> KbdEvent.Key -> Float
intensityOfKeyPress model key =
    Basics.min 100 (toFloat (OS.milisSinceKeyDown key model.os) / 100)
