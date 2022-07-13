module User.Interface.Instrument exposing (Model, init, update, viewSelectChord)

import Chord
import CommonTypes exposing (Inputs(..), Routes(..), Selectors)
import FormElementEvent
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode
import KbdEvent exposing (Key(..))
import Message exposing (Message(..))


type alias Model =
    { activeChordName : Chord.Names
    , activeChord : Chord.Chord
    , hasFocus : Inputs
    }



{- TODO How to return commands? -}


init : Model
init =
    { activeChordName = Chord.X_Chord
    , activeChord = Chord.none
    , hasFocus = NoInput
    }


update : Message -> Model -> Selectors -> ( Model, Cmd Message )
update msg model _ =
    case msg of
        Message.SelectChord chord ->
            ( { model | activeChordName = chord }, Cmd.none )

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
    let
        handleClose =
            Message.RequestPreviousUrl 1
    in
    H.div
        [ HA.class "fixed top-0 left-0 w-100 h-100 flex justify-center items-center theme-bg-fade-50"
        , HE.onClick handleClose
        ]
        [ H.div
            [ HA.class "mw5 mw6-ns hidden ba mv4 flex space-between flex-column"
            , HE.stopPropagationOn "click" (Json.Decode.succeed ( Message.Nevermind, True ))
            ]
            [ H.div [ HA.class "bg-near-black white flex space-around flex-row" ]
                [ H.h1 [ HA.class "f4 mv0 pv2 ph3" ]
                    [ H.text "Select a chord"
                    ]
                , H.button
                    [ HE.onClick handleClose
                    , HA.class "pointer bn bgn white"
                    ]
                    [ H.text "X" ]
                ]
            , H.div [ HA.class "f6 f5-ns lh-copy measure mv0 pa2 flex flex-column items-center" ]
                [ viewSelectChordForm model
                ]
            ]
        ]


viewSelectChordForm : Model -> H.Html Message.Message
viewSelectChordForm model =
    H.div [ HA.class "flex flex-row items-center" ]
        [ H.button
            [ HE.onClick (Message.SelectChord (Chord.previous model.activeChordName))
            , HA.type_ "button"
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
            [ H.text (Chord.nameToStr model.activeChordName)
            ]
        , H.select
            [ HA.id "chord-select"
            , HA.class "visually-hidden"
            , HA.value (String.fromInt (Chord.nameToInt model.activeChordName))
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
            [ HE.onClick (Message.SelectChord (Chord.next model.activeChordName))
            , HA.type_ "button"
            ]
            [ H.text "▶" ]
        ]
