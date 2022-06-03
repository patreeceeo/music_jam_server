port module Instrument exposing (Model, Msg, PathSegment, PortMessage(..), Voice, createInstrument, createInstrumentVoice, createMouseEvent, decodeInstrument, encodePortMessage, fretDistance, fretIndex, mouseOverVoice, pitchAtOffset, sendPortMessage, setCurrentPitch, update, view, viewStringAnimationDurationMS, viewStringAnimationValues)

import Array exposing (Array)
import Html
import Json.Decode as D
import Json.Encode as E
import Svg
import Svg.Attributes exposing (..)
import Svg.Events
import Utils exposing (joinNums, joinPoints)



-- TODO use Float unless it really has to be an Int? Or vice versa?
-- PORTS


type PortMessage
    = PlaySound { soundId : String, pitch : Float, volume : Float }
    | LogError String


port sendPortMessage : E.Value -> Cmd msg


encodePortMessage : PortMessage -> E.Value
encodePortMessage msg =
    case msg of
        PlaySound data ->
            E.object
                [ ( "type", E.string "playSound" )
                , ( "data"
                  , E.object
                        [ ( "soundId", E.string data.soundId )
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



-- MODEL


type alias Voice =
    { currentPitch : Float
    , currentVolume : Float
    , notes : Array Float
    }


type alias Model =
    { voices : Array Voice
    }


createInstrument : List Voice -> Model
createInstrument voices =
    { voices = Array.fromList voices
    }


createInstrumentVoice : List Float -> Voice
createInstrumentVoice notes =
    { currentPitch = 0
    , currentVolume = 0
    , notes = Array.fromList notes
    }


asVoiceIn : Int -> Model -> Voice -> Model
asVoiceIn index instrument voice =
    let
        newVoices =
            Array.set index voice instrument.voices
    in
    { instrument | voices = newVoices }


asCurrentPitchIn : Voice -> Float -> Voice
asCurrentPitchIn voice pitch =
    { voice | currentPitch = pitch }


setCurrentPitch : Model -> Int -> Float -> Model
setCurrentPitch instrument voiceIndex pitch =
    case Array.get voiceIndex instrument.voices of
        Just voice ->
            pitch
                |> asCurrentPitchIn voice
                |> asVoiceIn voiceIndex instrument

        Nothing ->
            instrument


-- DECODE JSON


decodeInstrument : D.Decoder Model
decodeInstrument =
    D.map Model
        (D.field "voices" decodeInstrumentVoices)


decodeInstrumentVoices : D.Decoder (Array Voice)
decodeInstrumentVoices =
    D.array
        (D.map3 Voice
            (D.field "currentPitch" D.float)
            (D.field "currentVolume" D.float)
            (D.field "notes" (D.array D.float))
        )



-- UPDATE


type Msg
    = MouseOverVoice Int Int MouseEvent


mouseOverVoice : Int -> Int -> MouseEvent -> Msg
mouseOverVoice index screenWidth event =
    MouseOverVoice index screenWidth event


pitchAtOffset : Int -> Int -> Model -> Int -> Result String Float
pitchAtOffset offset screenWidth instrument voiceIndex =
    let
        unscaledOffset =
            (toFloat offset / toFloat screenWidth) * instW

        noteIndex =
            fretIndex unscaledOffset
    in
    case Array.get voiceIndex instrument.voices of
        Just voice ->
            case Array.get noteIndex voice.notes of
                Just note ->
                    Ok note

                Nothing ->
                    Err ("OutOfRangeErr: no note corresponding to offset " ++ String.fromInt offset ++ " on voice index " ++ String.fromInt voiceIndex)

        Nothing ->
            Err ("OutOfRangeErr: no voice corresponding to index " ++ String.fromInt voiceIndex)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseOverVoice index screenWidth event ->
            if event.buttons > 0 then
                let
                    pitchResult =
                        pitchAtOffset event.offsetX screenWidth model index
                in
                case pitchResult of
                    Ok pitch ->
                        ( setCurrentPitch model (Debug.log "setting voice index " index) (Debug.log "to pitch " pitch)
                        , sendPortMessage (encodePortMessage (PlaySound { soundId = "acoustic-guitar", pitch = pitch, volume = 40 }))
                        )

                    Err errMsg ->
                        ( model, sendPortMessage (encodePortMessage (LogError errMsg)) )

            else
                ( model, Cmd.none )



-- VIEW


instW : Float
instW =
    2000


instH : Float
instH =
    200


instShadowH : Float
instShadowH =
    20


viewBox_ : String
viewBox_ =
    joinNums " " [ 0, 0, instW, instH + instShadowH ]


outerPolyPoints : String
outerPolyPoints =
    joinPoints [ [ 0, 0 ], [ instW, 0 ], [ instW, instH ], [ 0, instH ] ]


fretCount : Int
fretCount =
    24


k : Float
k =
    5.71584144995393e-2


fretDistance : Int -> Float
fretDistance index =
    instW * 1.3 * (1 - (e ^ (-k * toFloat index)))


fretIndex : Float -> Int
fretIndex distance =
    let
        antiLog =
            -1 * (distance - (instW * 1.3)) / (instW * 1.3)
    in
    floor (-1 * logBase e antiLog / k)



-- TODO probably a more efficient implementation, isn't there?


fretWidth : Int -> Float
fretWidth index =
    fretDistance (index + 1) - fretDistance index


fretPath : Int -> String
fretPath index =
    "M" ++ joinNums " " [ fretDistance index, 0, fretDistance index, instH ]


fretIndexes : List Int
fretIndexes =
    List.range 1 fretCount


fretPathList : List String
fretPathList =
    List.map fretPath fretIndexes


fretsPath : String
fretsPath =
    String.join " " fretPathList


isEqualRemainder : Int -> Int -> Int -> Bool
isEqualRemainder dividend divisor remainder =
    remainder == modBy divisor dividend


isInlayFret : Int -> Bool
isInlayFret index =
    List.any (isEqualRemainder index 12) [ 3, 5, 7, 9, 0 ]


inlayFretIndexes : List Int
inlayFretIndexes =
    List.filter (\index -> isInlayFret index) fretIndexes


viewInlays : List (Svg.Svg Msg)
viewInlays =
    List.map
        (\index ->
            Svg.circle
                [ cx (String.fromFloat (fretDistance index + fretWidth index / 2))
                , cy (String.fromFloat (instH / 2))
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


frets : Svg.Svg Msg
frets =
    Svg.path
        [ d fretsPath
        , strokeWidth "10"
        , stroke "url(#fret)"
        , style "filter: url(#dropshadowFret);"
        ]
        []


type alias MouseEvent =
    { offsetX : Int
    , offsetY : Int
    , buttons : Int
    }


createMouseEvent : Int -> Int -> Int -> MouseEvent
createMouseEvent offsetX offsetY buttons =
    { offsetX = offsetX
    , offsetY = offsetY
    , buttons = buttons
    }


onMouseOver : (MouseEvent -> msg) -> Svg.Attribute msg
onMouseOver event =
    Svg.Events.on "mouseover" (D.map event decodeMouseEvent)


decodeMouseEvent : D.Decoder MouseEvent
decodeMouseEvent =
    D.map3
        MouseEvent
        (D.field "offsetX" D.int)
        (D.field "offsetY" D.int)
        (D.field "buttons" D.int)


stringY : Int -> Float
stringY index =
    toFloat ((index + 1) * 28)


activeFretX : Voice -> Float
activeFretX voice =
    case Array.get 0 voice.notes of
        Just firstNote ->
          if voice.currentPitch > 0 then
            fretDistance (floor (voice.currentPitch - firstNote))
          else
            instW

        Nothing ->
          instW


type alias PathSegment =
    { command : String
    , points : List (List Float)
    }


joinAnimationValues : List (List PathSegment) -> String
joinAnimationValues values =
  String.join ";" (List.map (\segments -> joinPathSegments segments) values)


joinPathSegments : List PathSegment -> String
joinPathSegments segments =
    String.join " " (List.map (\seg -> seg.command ++ joinPoints seg.points) segments)


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



-- , String.join " "
--     ([ "M"
--      , joinPoints [ [ 0, stringY_ ], [ activeFretX_, stringY_ ] ]
--      , "q"
--      , joinPoints [ [ period / 2, (amplitude / 2) ], [ period, 0 ] ]
--      , "q"
--      , joinPoints [ [ period / 2, -(amplitude / 2) ], [ period, 0 ] ]
--      , "q"
--      , joinPoints [ [ period / 2, (amplitude / 2) ], [ period, 0 ] ]
--      , "q"
--      , joinPoints [ [ period / 2, -(amplitude / 2) ], [ period, 0 ] ]
--      , "q"
--      , joinPoints [ [ period / 2, amplitude / 2 ], [ period, 0 ] ]
--      ]
--         ++ viewStringAnimationValuesTail activeFretX_ instW_ period
--     )
-- ]


viewStringAnimationDurationMS : Float -> Float
viewStringAnimationDurationMS pitch =
    2000


viewStringAnimation : Voice -> Int -> Svg.Svg Msg
viewStringAnimation voice index =
    let
        values_ =
            viewStringAnimationValues (activeFretX voice) (stringY index) instW 20 20

        dur_ =
            viewStringAnimationDurationMS voice.currentPitch
    in
    Svg.animate [ attributeName "d", values (joinAnimationValues values_), dur (String.fromFloat dur_ ++ "ms"), repeatCount "indefinite" ] []


viewString : Voice -> Int -> Int -> Int -> Svg.Svg Msg
viewString voice index time screenWidth =
    Svg.path
        [ cursor "crosshair"
        , strokeWidth "2"
        , stroke "rgba(255, 255, 255, 0.5)"
        , fill "none"
        , onMouseOver (MouseOverVoice index screenWidth)
        , id ("instrument-voice-" ++ String.fromInt index)
        ]
        [ viewStringAnimation voice index ]


viewStrings : Model -> Int -> Int -> List (Svg.Svg Msg)
viewStrings model time screenWidth =
    List.indexedMap
        (\index voice -> viewString voice index time screenWidth)
        (Array.toList model.voices)


viewDebugVoiceNote : Int -> ( Int, Float ) -> Svg.Svg Msg
viewDebugVoiceNote voiceIndex indexedNote =
    let
        ( noteIndex, note ) =
            indexedNote
    in
    Svg.text_ [ y (String.fromFloat (stringY voiceIndex)), x (String.fromFloat (fretDistance noteIndex + 10.0)), fill "rgba(255, 255, 255, 0.5)", class "inert" ] [ Svg.text (String.fromFloat note) ]


viewDebugVoiceNotes : ( Int, Voice ) -> List (Svg.Svg Msg)
viewDebugVoiceNotes indexedVoice =
    let
        ( voiceIndex, voice ) =
            indexedVoice
    in
    List.map (viewDebugVoiceNote voiceIndex) (Array.toIndexedList voice.notes)


viewDebugNotes : Model -> List (Svg.Svg Msg)
viewDebugNotes model =
    List.concatMap viewDebugVoiceNotes (Array.toIndexedList model.voices)


viewDebugPlayingNotes : Model -> String
viewDebugPlayingNotes model =
    String.join " "
        (List.map (\voice -> String.fromFloat voice.currentPitch)
            (Array.toList model.voices)
        )


viewDebugging : Model -> List (Svg.Svg Msg)
viewDebugging model =
    viewDebugNotes model ++ [ Svg.text_ [ y "215" ] [ Svg.text ("playing notes " ++ viewDebugPlayingNotes model) ] ]


view : Model -> Int -> Int -> Html.Html Msg
view model time screenWidth =
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
            ++ viewStrings model time screenWidth
            ++ viewDebugging model
        )
