module User.Interface.Instrument exposing (Model, init, update, viewSelectChord)

import Chord
import CommonTypes exposing (Inputs(..))
import FormElementEvent
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Instrument
import Json.Decode
import Message exposing (Message(..))
import Selectors


type alias Model =
    { activeChord : Chord.Names
    , hasFocus : Inputs
    }



{- TODO How to return commands? -}


init : Maybe Instrument.Model -> Model
init maybeInstrument =
    { activeChord =
        case maybeInstrument of
            Just instrument ->
                case Chord.identify instrument.activeChord of
                    Ok name ->
                        name

                    Err errMsg ->
                        Debug.log errMsg Chord.X_Chord

            Nothing ->
                Chord.X_Chord
    , hasFocus = NoInput
    }


update : Message -> Model -> Selectors.Selectors -> ( Model, Cmd Message )
update msg model _ =
    case msg of
        Message.SelectChord chord ->
            ( { model | activeChord = chord }, Cmd.none )

        Message.FocusChange input ->
            ( { model | hasFocus = Debug.log "input" input }, Cmd.none )

        _ ->
            ( model, Cmd.none )


mapToChordMessage : (Chord.Names -> Message.Message) -> String -> Message.Message
mapToChordMessage tagger str =
    str
        |> Chord.decodeNames
        |> tagger


viewSelectChord : Model -> H.Html Message.Message
viewSelectChord model =
    H.div
        [ HA.class "fixed top-0 left-0 w-100 h-100 flex justify-center items-center theme-bg-fade-50"
        , HE.onClick (Message.RequestPreviousUrl 1)
        ]
        [ H.div
            [ HA.class "mw5 mw6-ns hidden ba mv4"
            , HE.stopPropagationOn "click" (Json.Decode.succeed ( Message.Nevermind, True ))
            ]
            [ H.h1 [ HA.class "f4 bg-near-black white mv0 pv2 ph3" ]
                [ H.text "Select a chord" ]
            , H.div [ HA.class "f6 f5-ns lh-copy measure mv0 pa2 flex flex-column items-center" ]
                [ H.div [ HA.class "flex flex-row items-center" ]
                    [ H.button
                        [ HE.onClick (Message.SelectChord (Chord.previous model.activeChord))
                        ]
                        [ H.text "◀" ]
                    , H.label
                        [ HA.class
                            ((if model.hasFocus == SelectChordInput then
                                "theme-focus-ring"

                              else
                                ""
                             )
                                ++ " pointer ba b--inset bw2 dib w3 tc"
                            )
                        , HA.for "chord-select"
                        ]
                        [ H.text (Chord.nameToStr model.activeChord)
                        ]
                    , H.select
                        [ HA.id "chord-select"
                        , HA.class "visually-hidden"
                        , Message.FocusChange SelectChordInput
                            |> Json.Decode.succeed
                            |> HE.on "focus"
                        , Message.FocusChange NoInput
                            |> Json.Decode.succeed
                            |> HE.on "blur"
                        , Message.SelectChord
                            |> mapToChordMessage
                            |> FormElementEvent.decodeValue
                            |> HE.on "change"
                        ]
                        (List.map (\( int, string ) -> H.option [ HA.value (String.fromInt int) ] [ H.text string ]) Chord.nameIntStrPairs)
                    , H.button
                        [ HE.onClick (Message.SelectChord (Chord.next model.activeChord))
                        ]
                        [ H.text "▶" ]
                    ]
                , H.button
                    [ HE.onClick (Message.RequestPreviousUrl 1)
                    ]
                    [ H.text "OK" ]
                ]
            ]
        ]
