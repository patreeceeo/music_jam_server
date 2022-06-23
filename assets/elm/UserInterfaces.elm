module UserInterfaces exposing (instrument, navMenu, viewStringAnimationValues)

import Array
import Html
import Html.Attributes as Hattr
import Html.Events
import Instrument
import Message
import MouseEvent
import OperatingSystem as OS
import Svg
import Svg.Attributes as Sattr
import Svg.Events
import Utils exposing (PathSegment, joinAnimationValues, joinNums, joinPoints, loopInt)


viewBox : String
viewBox =
    joinNums " " [ 0, 0, Instrument.width, Instrument.height + instShadowH ]


instrument : Instrument.Model -> OS.Model -> (Int -> MouseEvent.Model -> Message.Message) -> Html.Html Message.Message
instrument instrumentModel osModel msgForMouseOver =
    Html.div []
        [ Svg.svg
            [ Sattr.class "instrument"
            , Sattr.preserveAspectRatio "xMidYMid meet"
            , Sattr.viewBox viewBox
            ]
            ([ svgDefs
             , outerPoly
             , frets
             ]
                ++ viewInlays
                ++ viewStrings instrumentModel osModel msgForMouseOver
                ++ viewDebugging instrumentModel
            )
        , controls
        ]


instShadowH : Float
instShadowH =
    20


outerPolyPoints : String
outerPolyPoints =
    joinPoints [ [ 0, 0 ], [ Instrument.width, 0 ], [ Instrument.width, Instrument.height ], [ 0, Instrument.height ] ]


viewInlays : List (Svg.Svg msg)
viewInlays =
    List.map
        (\index ->
            Svg.circle
                [ Sattr.cx (String.fromFloat (Instrument.fretDistance index + Instrument.fretWidth index / 2))
                , Sattr.cy (String.fromFloat (Instrument.height / 2))
                , Sattr.r "16"
                , Sattr.fill "rgba(255, 255, 255, 0.1)"
                ]
                []
        )
        inlayFretIndexes


svgDefs : Svg.Svg msg
svgDefs =
    Svg.defs []
        [ Svg.linearGradient
            [ Sattr.id "fretboard"
            , Sattr.x1 "42%"
            , Sattr.y1 "100%"
            , Sattr.x2 "0%"
            , Sattr.y2 "90%"
            ]
            [ Svg.stop
                [ Sattr.offset "0%"
                , Sattr.style "stop-color: rgb(56, 53, 53);"
                ]
                []
            , Svg.stop
                [ Sattr.offset "100%"
                , Sattr.style "stop-color: rgb(76, 73, 73);"
                ]
                []
            ]
        , Svg.linearGradient
            [ Sattr.id "fret"
            , Sattr.x1 "0%"
            , Sattr.y1 "0%"
            , Sattr.x2 "100%"
            , Sattr.y2 "0%"
            ]
            [ Svg.stop
                [ Sattr.offset "0%"
                , Sattr.style "stop-color: rgb(160, 140, 130);"
                ]
                []
            , Svg.stop
                [ Sattr.offset "100%"
                , Sattr.style "stop-color: rgb(120, 100, 90);"
                ]
                []
            ]
        , Svg.filter
            [ Sattr.id "dropshadowFretboard"
            , Sattr.height "400%"
            ]
            [ Svg.feGaussianBlur
                [ Sattr.in_ "SourceAlpha"
                , Sattr.stdDeviation "3"
                ]
                []
            , Svg.feOffset
                [ Sattr.dx "4"
                , Sattr.dy "4"
                , Sattr.result "offsetblur"
                ]
                []
            , Svg.feComponentTransfer []
                [ Svg.feFuncA [ Sattr.slope "0.5" ] []
                ]
            , Svg.feMerge []
                [ Svg.feMergeNode [] []
                , Svg.feMergeNode
                    [ Sattr.in_ "SourceGraphic"
                    ]
                    []
                ]
            ]
        , Svg.filter
            [ Sattr.id "dropshadowFret"
            , Sattr.height "400%"
            ]
            [ Svg.feGaussianBlur
                [ Sattr.in_ "SourceAlpha"
                , Sattr.stdDeviation "3"
                ]
                []
            , Svg.feOffset
                [ Sattr.dx "2"
                , Sattr.dy "2"
                , Sattr.result "offsetblur"
                ]
                []
            , Svg.feComponentTransfer []
                [ Svg.feFuncA
                    [ Sattr.slope "0.3"
                    ]
                    []
                ]
            , Svg.feMerge [] [ Svg.feMergeNode [] [], Svg.feMergeNode [ Sattr.in_ "SourceGraphic" ] [] ]
            ]
        ]


outerPoly : Svg.Svg msg
outerPoly =
    Svg.polygon
        [ Sattr.points outerPolyPoints
        , Sattr.fill "url(#fretboard)"
        , Sattr.style "filter: url(#dropshadowFretboard);"
        , Sattr.strokeLinejoin "round"
        ]
        []


fretPath : Int -> String
fretPath index =
    "M" ++ joinNums " " [ Instrument.fretDistance index, 0, Instrument.fretDistance index, Instrument.height ]


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


frets : Svg.Svg msg
frets =
    Svg.path
        [ Sattr.d fretsPath
        , Sattr.strokeWidth "10"
        , Sattr.stroke "url(#fret)"
        , Sattr.style "filter: url(#dropshadowFret);"
        ]
        []


activeFretX : Instrument.Voice -> Float
activeFretX voice =
    case Array.get 0 voice.notes of
        Just firstNote ->
            if voice.currentPitch > 0 then
                Instrument.fretDistance (floor (voice.currentPitch - firstNote))

            else
                Instrument.width

        Nothing ->
            Instrument.width


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


viewStringAnimation : Instrument.Voice -> Int -> Int -> Svg.Svg msg
viewStringAnimation voice index time =
    let
        timeElapsed =
            time - voice.lastNoteStartTime

        amplitude =
            sin (radians (pi * (toFloat stringAmplitudeCycleDurationMS / toFloat (loopInt 0 stringAmplitudeCycleDurationMS timeElapsed)))) * stringMaxAmplitudePX * toFloat (noteDurationMS - timeElapsed) / toFloat noteDurationMS * voice.currentVolume

        activeFretX_ =
            if timeElapsed < noteDurationMS then
                activeFretX voice

            else
                Instrument.width

        values_ =
            viewStringAnimationValues activeFretX_ (stringY index) Instrument.width (44000 / (voice.currentPitch * voice.currentPitch)) amplitude
    in
    Svg.animate [ Sattr.attributeName "d", Sattr.values (joinAnimationValues values_), Sattr.dur (String.fromInt stringAnimationDurationMS ++ "ms"), Sattr.repeatCount "indefinite" ] []


viewString : Instrument.Voice -> Int -> Int -> (MouseEvent.Model -> msg) -> Svg.Svg msg
viewString voice index time msgForMouseOver =
    Svg.path
        [ Sattr.cursor "crosshair"
        , Sattr.strokeWidth "2"
        , Sattr.stroke "rgba(255, 255, 255, 0.5)"
        , Sattr.fill "none"
        , onMouseOver msgForMouseOver
        , Sattr.id ("instrument-voice-" ++ String.fromInt index)
        ]
        [ viewStringAnimation voice index time ]


viewStrings : Instrument.Model -> OS.Model -> (Int -> MouseEvent.Model -> msg) -> List (Svg.Svg msg)
viewStrings instrumentModel osModel msgForMouseOver =
    List.indexedMap
        (\index voice -> viewString voice index osModel.timeInMillis (msgForMouseOver index))
        (Array.toList instrumentModel.voices)


viewDebugVoiceNote : Int -> ( Int, Float ) -> Svg.Svg msg
viewDebugVoiceNote voiceIndex indexedNote =
    let
        ( noteIndex, note ) =
            indexedNote
    in
    Svg.text_ [ Sattr.y (String.fromFloat (stringY voiceIndex)), Sattr.x (String.fromFloat (Instrument.fretDistance noteIndex + 10.0)), Sattr.fill "rgba(255, 255, 255, 0.5)", Sattr.class "inert" ] [ Svg.text (String.fromFloat note) ]


viewDebugVoiceNotes : ( Int, Instrument.Voice ) -> List (Svg.Svg msg)
viewDebugVoiceNotes indexedVoice =
    let
        ( voiceIndex, voice ) =
            indexedVoice
    in
    List.map (viewDebugVoiceNote voiceIndex) (Array.toIndexedList voice.notes)


viewDebugNotes : Instrument.Model -> List (Svg.Svg msg)
viewDebugNotes model =
    List.concatMap viewDebugVoiceNotes (Array.toIndexedList model.voices)


viewDebugPlayingNotes : Instrument.Model -> String
viewDebugPlayingNotes model =
    String.join " "
        (List.map (\voice -> String.fromFloat voice.currentPitch)
            (Array.toList model.voices)
        )


viewDebugging : Instrument.Model -> List (Svg.Svg msg)
viewDebugging model =
    viewDebugNotes model ++ [ Svg.text_ [ Sattr.y "215" ] [ Svg.text ("playing notes " ++ viewDebugPlayingNotes model) ] ]


onMouseOver : (MouseEvent.Model -> msg) -> Svg.Attribute msg
onMouseOver event =
    Svg.Events.on "mouseover" (MouseEvent.mapDecode event)


controls : Html.Html Message.Message
controls =
    Html.button
        [ Html.Events.onClick (Message.PlayChord 0.5)
        ]
        [ Html.text "strum" ]


navMenu : Html.Html Message.Message
navMenu =
    Html.nav []
        [ Html.a [ Hattr.href "/lab/fretboard" ] [ Html.text "home" ]
        , Html.a [ Hattr.href "/faq" ] [ Html.text "FAQs" ]
        ]
