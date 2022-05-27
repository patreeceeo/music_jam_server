module Main exposing (main)

-- IN-HOUSE MODULES
-- STDLIB MODULES

import Browser
import Browser.Events
import Html
import Instrument
import Json.Decode as D
import Time



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
    { instrument : Instrument.Model }


type alias Model =
    { timeInMillis : Int
    , instrument : Maybe Instrument.Model
    }


init : D.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue decodeFlags flags of
        Ok decodedFlags ->
            { timeInMillis = 0
            , instrument = Just (.instrument decodedFlags)
            }

        Err _ ->
            { timeInMillis = 0, instrument = Nothing }
    , Cmd.none
    )



-- setCurrentPitchInModel : Model -> Int -> Float -> Model
-- setCurrentPitchInModel model voiceIndex pitch =
--     case model.instrument of
--         Just instrument ->
--           Instrument.setCurrentPitch instrument voiceIndex pitch
--           |> asInstrumentIn model
--         Nothing ->
--             model
-- asInstrumentIn : Model -> Instrument -> Model
-- asInstrumentIn model instrument =
--     { model | instrument = Just instrument }
-- DECODE JSON


decodeFlags : D.Decoder Flags
decodeFlags =
    D.map Flags
        (D.field "instrument" Instrument.decodeInstrument)



-- UPDATE


type Msg
    = AnimationFrame Time.Posix
    | InstrumentMsg Instrument.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.instrument of
        Just instrument ->
            case msg of
                AnimationFrame newTime ->
                    ( { model
                        | timeInMillis = Time.posixToMillis newTime
                      }
                    , Cmd.none
                    )

                InstrumentMsg instrumentMsg ->
                    let
                        ( updatedInstrument, instrumentCmd ) =
                            Instrument.update instrumentMsg instrument
                    in
                    ( { model | instrument = Just updatedInstrument }, Cmd.map InstrumentMsg instrumentCmd )

        Nothing ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame AnimationFrame



-- VIEW


view : Model -> Html.Html Msg
view model =
    case model.instrument of
        Just instrument ->
            Html.div []
                [ Html.map InstrumentMsg (Instrument.view instrument model.timeInMillis)
                ]

        Nothing ->
            Html.p [] [ Html.text "Uh oh there was an error! Looks like the programmers goofed up the JSON encoding/decoding" ]
