port module Main exposing (main)

-- IN-HOUSE MODULES
-- STDLIB MODULES

import Browser
import Browser.Events
import Html
import Instrument exposing (createInstrument, createInstrumentVoice, subscriptionBatch, encodePortMessage, PortMessage(..))
import Json.Decode as D
import Time
import Json.Encode as E


port sendPortMessage : E.Value -> Cmd msg
port receivePortMessage : (E.Value -> msg) -> Sub msg

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
    { timeInMillis : Int
    , screenWidth : Int
    , instrument : Maybe Instrument.Model
    }


defaultInstrument : Instrument.Model
defaultInstrument =
    createInstrument
        [ createInstrumentVoice
            [ 64
            , 65
            , 66
            , 67
            , 68
            , 69
            , 70
            , 71
            , 72
            , 73
            , 74
            , 75
            , 76
            , 77
            , 78
            , 79
            , 80
            , 81
            , 82
            , 83
            , 84
            , 85
            , 86
            , 87
            , 88
            ]
        , createInstrumentVoice
            [ 59
            , 60
            , 61
            , 62
            , 63
            , 64
            , 65
            , 66
            , 67
            , 68
            , 69
            , 70
            , 71
            , 72
            , 73
            , 74
            , 75
            , 76
            , 77
            , 78
            , 79
            , 80
            , 81
            , 82
            , 83
            ]
        , createInstrumentVoice
            [ 55
            , 56
            , 57
            , 58
            , 59
            , 60
            , 61
            , 62
            , 63
            , 64
            , 65
            , 66
            , 67
            , 68
            , 69
            , 70
            , 71
            , 72
            , 73
            , 74
            , 75
            , 76
            , 77
            , 78
            , 79
            ]
        , createInstrumentVoice
            [ 50
            , 51
            , 52
            , 53
            , 54
            , 55
            , 56
            , 57
            , 58
            , 59
            , 60
            , 61
            , 62
            , 63
            , 64
            , 65
            , 66
            , 67
            , 68
            , 69
            , 70
            , 71
            , 72
            , 73
            , 74
            ]
        , createInstrumentVoice
            [ 45
            , 46
            , 47
            , 48
            , 49
            , 50
            , 51
            , 52
            , 53
            , 54
            , 55
            , 56
            , 57
            , 58
            , 59
            , 60
            , 61
            , 62
            , 63
            , 64
            , 65
            , 66
            , 67
            , 68
            , 69
            ]
        , createInstrumentVoice
            [ 40
            , 41
            , 42
            , 43
            , 44
            , 45
            , 46
            , 47
            , 48
            , 49
            , 50
            , 51
            , 52
            , 53
            , 54
            , 55
            , 56
            , 57
            , 58
            , 59
            , 60
            , 61
            , 62
            , 63
            , 64
            ]
        ]


init : D.Value -> ( Model, Cmd Msg )
init flags =
    case D.decodeValue decodeFlags flags of
        Ok decodedFlags ->
            ({ timeInMillis = 0
            , screenWidth = decodedFlags.screenWidth
            , instrument = Just (.instrument decodedFlags)
            },
            Cmd.none
            )

        Err errMsg ->
            ({ timeInMillis = 0, screenWidth = 0, instrument = Just defaultInstrument }
            , sendPortMessage (encodePortMessage (LogError (D.errorToString errMsg)))
            )
    


decodeFlags : D.Decoder Flags
decodeFlags =
    D.map2 Flags
        (D.field "screenWidth" D.int)
        (D.field "instrument" Instrument.decodeInstrument)



-- UPDATE


type Msg
    = AnimationFrame Time.Posix
    | InstrumentMsg Instrument.Msg
    | WindowResize Int


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

                WindowResize width ->
                    ( { model | screenWidth = width }
                    , Cmd.none
                    )

        Nothing ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        ([ Browser.Events.onAnimationFrame AnimationFrame
        , Browser.Events.onResize (\w _ -> WindowResize w)
        ] ++ (List.map (\sub -> Sub.map InstrumentMsg sub) subscriptionBatch)) 



-- VIEW


view : Model -> Html.Html Msg
view model =
    case model.instrument of
        Just instrument ->
            Html.div []
                [ Html.map InstrumentMsg (Instrument.view instrument model.timeInMillis model.screenWidth)
                ]

        Nothing ->
            Html.p [] [ Html.text "Uh oh there was an error! Looks like the programmers goofed up the JSON encoding/decoding" ]
