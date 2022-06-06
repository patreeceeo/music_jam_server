port module Main exposing (Model, main, update, view, mouseOverVoice, encodePortMessage, sendPortMessage, viewStringAnimationValues, PortMessage(..))

import Array
import Browser
import Browser.Events
import Html
import Instrument
import Json.Decode as D
import Json.Encode as E
import Svg
import Svg.Attributes exposing (..)
import Svg.Events
import Time
import Utils exposing (PathSegment, joinAnimationValues, joinNums, joinPoints, loopInt)
import MouseEvent



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TODO use Float unless it really has to be an Int? Or vice versa?
-- PORTS


type alias PlaySoundWrapper =
    { type_ : String, data : PlaySoundData }


type alias PlaySoundData =
    { soundId : String, voiceIndex : Int, pitch : Float, volume : Float }


type PortMessage
    = PlaySound PlaySoundData
    | LogError String


port sendPortMessage : E.Value -> Cmd msg


port receivePortMessage : (E.Value -> msg) -> Sub msg


encodePortMessage : PortMessage -> E.Value
encodePortMessage msg =
    case msg of
        PlaySound data ->
            E.object
                [ ( "type", E.string "playSound" )
                , ( "data"
                  , E.object
                        [ ( "soundId", E.string data.soundId )
                        , ( "voiceIndex", E.int data.voiceIndex )
                        , ( "pitch", E.float data.pitch )
                        , ( "volume", E.float data.volume )
                        ]
                  )
                ]

        LogError errMsg ->
            E.object
                [ ( "type", E.string "logError" )
                , ( "data"
                  , E.object
                        [ ( "message", E.string errMsg ) ]
                  )
                ]


decodePortMessage : D.Decoder PlaySoundWrapper
decodePortMessage =
    D.at [ "type" ] D.string
        |> D.andThen decodePortMessageData


decodePortMessageData : String -> D.Decoder PlaySoundWrapper
decodePortMessageData type_ =
    case type_ of
        "playSound" ->
            decodePlaySoundMessage

        _ ->
            D.fail ("unhandled message type " ++ type_)


decodePlaySoundMessage : D.Decoder PlaySoundWrapper
decodePlaySoundMessage =
    D.map2 PlaySoundWrapper
        (D.field "type" D.string)
        (D.field "data" decodePlaySoundMessageData)


decodePlaySoundMessageData : D.Decoder PlaySoundData
decodePlaySoundMessageData =
    D.map4 PlaySoundData
        (D.field "soundId" D.string)
        (D.field "voiceIndex" D.int)
        (D.field "pitch" D.float)
        (D.field "volume" D.float)



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


init : D.Value -> ( Model, Cmd Msg )
init flags =
    case D.decodeValue decodeFlags flags of
        Ok decodedFlags ->
            ( { timeInMillis = 0
              , screenWidth = decodedFlags.screenWidth
              , instrument = Just (.instrument decodedFlags)
              }
            , Cmd.none
            )

        Err errMsg ->
            ( { timeInMillis = 0, screenWidth = 0, instrument = Nothing }
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
    | WindowResize Int
    | MouseOverVoice Int Int Int MouseEvent.Model
    | ReceivePortMessage E.Value


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

                WindowResize width ->
                    ( { model | screenWidth = width }
                    , Cmd.none
                    )

                MouseOverVoice index screenWidth when event ->
                    if event.buttons > 0 then
                        let
                            pitchResult =
                                Instrument.pitchAtOffset event.offsetX screenWidth instrument index
                        in
                        case pitchResult of
                            Ok pitch ->
                                ( { model | instrument = Just (Instrument.playNote instrument index pitch 40 when) }
                                , sendPortMessage (encodePortMessage (PlaySound { soundId = "acoustic-guitar", voiceIndex = index, pitch = pitch, volume = 40 }))
                                )

                            Err errMsg ->
                                ( model, sendPortMessage (encodePortMessage (LogError errMsg)) )

                    else
                        ( model, Cmd.none )

                ReceivePortMessage msgJson ->
                    case D.decodeValue decodePortMessage msgJson of
                        Ok playSound ->
                            ( { model | instrument = Just (Instrument.playNote instrument playSound.data.voiceIndex playSound.data.pitch 40 model.timeInMillis) }
                            , Cmd.none
                            )

                        Err error ->
                            ( model
                            , sendPortMessage (encodePortMessage (LogError (D.errorToString error)))
                            )

        Nothing ->
            ( model, Cmd.none )


mouseOverVoice : Int -> Int -> Int -> MouseEvent.Model -> Msg
mouseOverVoice index screenWidth when event =
    MouseOverVoice index screenWidth when event



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame AnimationFrame
        , Browser.Events.onResize (\w _ -> WindowResize w)
        , receivePortMessage ReceivePortMessage
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    case model.instrument of
        Just instrument ->
            Svg.svg
                [ class "instrument"
                , preserveAspectRatio "xMidYMid meet"
                , viewBox viewBox_
                ]
                ([ svgDefs
                 , outerPoly
                 , frets
                 ]
                    ++ viewInlays
                    ++ viewStrings model
                    ++ viewDebugging instrument
                )

        Nothing ->
            Html.p [] [ Html.text "Uh oh there was an error! Looks like the programmers goofed up the JSON encoding/decoding" ]


instShadowH : Float
instShadowH =
    20


viewBox_ : String
viewBox_ =
    joinNums " " [ 0, 0, Instrument.instW, Instrument.instH + instShadowH ]


outerPolyPoints : String
outerPolyPoints =
    joinPoints [ [ 0, 0 ], [ Instrument.instW, 0 ], [ Instrument.instW, Instrument.instH ], [ 0, Instrument.instH ] ]


viewInlays : List (Svg.Svg Msg)
viewInlays =
    List.map
        (\index ->
            Svg.circle
                [ cx (String.fromFloat (Instrument.fretDistance index + Instrument.fretWidth index / 2))
                , cy (String.fromFloat (Instrument.instH / 2))
                , r "16"
                , fill "rgba(255, 255, 255, 0.1)"
                ]
                []
        )
        inlayFretIndexes


svgDefs : Svg.Svg Msg
svgDefs =
    Svg.defs []
        [ Svg.linearGradient
            [ id "fretboard"
            , x1 "42%"
            , y1 "100%"
            , x2 "0%"
            , y2 "90%"
            ]
            [ Svg.stop
                [ offset "0%"
                , style "stop-color: rgb(56, 53, 53);"
                ]
                []
            , Svg.stop
                [ offset "100%"
                , style "stop-color: rgb(76, 73, 73);"
                ]
                []
            ]
        , Svg.linearGradient
            [ id "fret"
            , x1 "0%"
            , y1 "0%"
            , x2 "100%"
            , y2 "0%"
            ]
            [ Svg.stop
                [ offset "0%"
                , style "stop-color: rgb(160, 140, 130);"
                ]
                []
            , Svg.stop
                [ offset "100%"
                , style "stop-color: rgb(120, 100, 90);"
                ]
                []
            ]
        , Svg.filter
            [ id "dropshadowFretboard"
            , height "400%"
            ]
            [ Svg.feGaussianBlur
                [ in_ "SourceAlpha"
                , stdDeviation "3"
                ]
                []
            , Svg.feOffset
                [ dx "4"
                , dy "4"
                , result "offsetblur"
                ]
                []
            , Svg.feComponentTransfer []
                [ Svg.feFuncA [ slope "0.5" ] []
                ]
            , Svg.feMerge []
                [ Svg.feMergeNode [] []
                , Svg.feMergeNode
                    [ in_ "SourceGraphic"
                    ]
                    []
                ]
            ]
        , Svg.filter
            [ id "dropshadowFret"
            , height "400%"
            ]
            [ Svg.feGaussianBlur
                [ in_ "SourceAlpha"
                , stdDeviation "3"
                ]
                []
            , Svg.feOffset
                [ dx "2"
                , dy "2"
                , result "offsetblur"
                ]
                []
            , Svg.feComponentTransfer []
                [ Svg.feFuncA
                    [ slope "0.3"
                    ]
                    []
                ]
            , Svg.feMerge [] [ Svg.feMergeNode [] [], Svg.feMergeNode [ in_ "SourceGraphic" ] [] ]
            ]
        ]


outerPoly : Svg.Svg Msg
outerPoly =
    Svg.polygon
        [ points outerPolyPoints
        , fill "url(#fretboard)"
        , style "filter: url(#dropshadowFretboard);"
        , strokeLinejoin "round"
        ]
        []


fretPath : Int -> String
fretPath index =
    "M" ++ joinNums " " [ Instrument.fretDistance index, 0, Instrument.fretDistance index, Instrument.instH ]


fretIndexes : List Int
fretIndexes =
    List.range 1 Instrument.fretCount


inlayFretIndexes : List Int
inlayFretIndexes =
    List.filter (\index -> Instrument.isInlayFret index) fretIndexes


fretPathList : List String
fretPathList =
    List.map fretPath fretIndexes


fretsPath : String
fretsPath =
    String.join " " fretPathList


frets : Svg.Svg Msg
frets =
    Svg.path
        [ d fretsPath
        , strokeWidth "10"
        , stroke "url(#fret)"
        , style "filter: url(#dropshadowFret);"
        ]
        []


activeFretX : Instrument.Voice -> Float
activeFretX voice =
    case Array.get 0 voice.notes of
        Just firstNote ->
            if voice.currentPitch > 0 then
                Instrument.fretDistance (floor (voice.currentPitch - firstNote))

            else
                Instrument.instW

        Nothing ->
            Instrument.instW


stringY : Int -> Float
stringY index =
    toFloat ((index + 1) * 28)


viewStringAnimationValuesTail : Float -> Float -> Float -> List PathSegment
viewStringAnimationValuesTail activeFretX_ instW_ period =
    let
        segmentCount =
            ceiling ((instW_ - activeFretX_) / period) - 1

        segmentIndexes =
            List.range 0 (segmentCount - 1)
    in
    List.map (\_ -> PathSegment "t" [ [ period, 0 ] ]) segmentIndexes


viewStringAnimationValues : Float -> Float -> Float -> Float -> Float -> List (List PathSegment)
viewStringAnimationValues activeFretX_ stringY_ instW_ period amplitude =
    [ [ PathSegment "M" [ [ 0, stringY_ ], [ activeFretX_, stringY_ ] ]
      , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
      , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
      , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
      , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
      , PathSegment "q" [ [ period / 2, amplitude / 2 ], [ period, 0 ] ]
      ]
        ++ viewStringAnimationValuesTail activeFretX_ instW_ period
    , [ PathSegment "M" [ [ 0, stringY_ ], [ activeFretX_, stringY_ ] ]
      , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
      , PathSegment "q" [ [ 0, 0 ], [ 0, 0 ] ]
      , PathSegment "q" [ [ period / 2, amplitude / 2 ], [ period, 0 ] ]
      , PathSegment "q" [ [ period / 2, -(amplitude / 2) ], [ period, 0 ] ]
      , PathSegment "q" [ [ period / 2, amplitude / 2 ], [ period, 0 ] ]
      ]
        ++ viewStringAnimationValuesTail activeFretX_ instW_ period
    , [ PathSegment "M" [ [ 0, stringY_ ], [ activeFretX_, stringY_ ] ]
      , PathSegment "q" [ [ period / 2, amplitude / 2 ], [ period, 0 ] ]
      , PathSegment "q" [ [ period / 2, -(amplitude / 2) ], [ period, 0 ] ]
      , PathSegment "q" [ [ period / 2, amplitude / 2 ], [ period, 0 ] ]
      , PathSegment "q" [ [ period / 2, -(amplitude / 2) ], [ period, 0 ] ]
      , PathSegment "q" [ [ period / 2, amplitude / 2 ], [ period, 0 ] ]
      ]
        ++ viewStringAnimationValuesTail activeFretX_ instW_ period
    ]


stringAnimationDurationMS : Int
stringAnimationDurationMS =
    200


stringAmplitudeCycleDurationMS : Int
stringAmplitudeCycleDurationMS =
    1000


stringMaxAmplitudePX : Float
stringMaxAmplitudePX =
    20


noteDurationMS : Int
noteDurationMS =
    5000


viewStringAnimation : Instrument.Voice -> Int -> Int -> Svg.Svg Msg
viewStringAnimation voice index time =
    let
        timeElapsed =
            time - voice.lastNoteStartTime

        amplitude =
            sin (radians (pi * (toFloat stringAmplitudeCycleDurationMS / toFloat (loopInt 0 stringAmplitudeCycleDurationMS timeElapsed)))) * stringMaxAmplitudePX * toFloat (noteDurationMS - timeElapsed) / toFloat noteDurationMS

        activeFretX_ =
            if timeElapsed < noteDurationMS then
                activeFretX voice

            else
                Instrument.instW

        values_ =
            viewStringAnimationValues activeFretX_ (stringY index) Instrument.instW (44000 / (voice.currentPitch * voice.currentPitch)) amplitude
    in
    Svg.animate [ attributeName "d", values (joinAnimationValues values_), dur (String.fromInt stringAnimationDurationMS ++ "ms"), repeatCount "indefinite" ] []


viewString : Instrument.Voice -> Int -> Int -> Int -> Svg.Svg Msg
viewString voice index time screenWidth =
    Svg.path
        [ cursor "crosshair"
        , strokeWidth "2"
        , stroke "rgba(255, 255, 255, 0.5)"
        , fill "none"
        , onMouseOver (MouseOverVoice index screenWidth time)
        , id ("instrument-voice-" ++ String.fromInt index)
        ]
        [ viewStringAnimation voice index time ]


viewStrings : Model -> List (Svg.Svg Msg)
viewStrings model =
    case model.instrument of
        Just instrument ->
            List.indexedMap
                (\index voice -> viewString voice index model.timeInMillis model.screenWidth)
                (Array.toList instrument.voices)

        Nothing ->
            []


viewDebugVoiceNote : Int -> ( Int, Float ) -> Svg.Svg Msg
viewDebugVoiceNote voiceIndex indexedNote =
    let
        ( noteIndex, note ) =
            indexedNote
    in
    Svg.text_ [ y (String.fromFloat (stringY voiceIndex)), x (String.fromFloat (Instrument.fretDistance noteIndex + 10.0)), fill "rgba(255, 255, 255, 0.5)", class "inert" ] [ Svg.text (String.fromFloat note) ]


viewDebugVoiceNotes : ( Int, Instrument.Voice ) -> List (Svg.Svg Msg)
viewDebugVoiceNotes indexedVoice =
    let
        ( voiceIndex, voice ) =
            indexedVoice
    in
    List.map (viewDebugVoiceNote voiceIndex) (Array.toIndexedList voice.notes)


viewDebugNotes : Instrument.Model -> List (Svg.Svg Msg)
viewDebugNotes model =
    List.concatMap viewDebugVoiceNotes (Array.toIndexedList model.voices)


viewDebugPlayingNotes : Instrument.Model -> String
viewDebugPlayingNotes model =
    String.join " "
        (List.map (\voice -> String.fromFloat voice.currentPitch)
            (Array.toList model.voices)
        )


viewDebugging : Instrument.Model -> List (Svg.Svg Msg)
viewDebugging model =
    viewDebugNotes model ++ [ Svg.text_ [ y "215" ] [ Svg.text ("playing notes " ++ viewDebugPlayingNotes model) ] ]


onMouseOver : (MouseEvent.Model -> msg) -> Svg.Attribute msg
onMouseOver event =
    Svg.Events.on "mouseover" (D.map event MouseEvent.decode)


