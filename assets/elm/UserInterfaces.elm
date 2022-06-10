module UserInterfaces exposing (instrument, viewStringAnimationValues)

import Array
import Html
import Instrument
import MouseEvent
import OperatingSystem as OS
import Svg
import Svg.Attributes as SvgA
import Svg.Events
import Utils exposing (PathSegment, joinAnimationValues, joinNums, joinPoints, loopInt)


viewBox : String
viewBox =
    joinNums " " [ 0, 0, Instrument.width, Instrument.height + instShadowH ]


instrument : Instrument.Model -> OS.Model -> (Int -> MouseEvent.Model -> msg) -> Html.Html msg
instrument instrumentModel osModel msgForMouseOver =
    Svg.svg
        [ SvgA.class "instrument"
        , SvgA.preserveAspectRatio "xMidYMid meet"
        , SvgA.viewBox viewBox
        ]
        ([ svgDefs
         , outerPoly
         , frets
         ]
            ++ viewInlays
            ++ viewStrings instrumentModel osModel msgForMouseOver
            ++ viewDebugging instrumentModel
        )


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
                [ SvgA.cx (String.fromFloat (Instrument.fretDistance index + Instrument.fretWidth index / 2))
                , SvgA.cy (String.fromFloat (Instrument.height / 2))
                , SvgA.r "16"
                , SvgA.fill "rgba(255, 255, 255, 0.1)"
                ]
                []
        )
        inlayFretIndexes


svgDefs : Svg.Svg msg
svgDefs =
    Svg.defs []
        [ Svg.linearGradient
            [ SvgA.id "fretboard"
            , SvgA.x1 "42%"
            , SvgA.y1 "100%"
            , SvgA.x2 "0%"
            , SvgA.y2 "90%"
            ]
            [ Svg.stop
                [ SvgA.offset "0%"
                , SvgA.style "stop-color: rgb(56, 53, 53);"
                ]
                []
            , Svg.stop
                [ SvgA.offset "100%"
                , SvgA.style "stop-color: rgb(76, 73, 73);"
                ]
                []
            ]
        , Svg.linearGradient
            [ SvgA.id "fret"
            , SvgA.x1 "0%"
            , SvgA.y1 "0%"
            , SvgA.x2 "100%"
            , SvgA.y2 "0%"
            ]
            [ Svg.stop
                [ SvgA.offset "0%"
                , SvgA.style "stop-color: rgb(160, 140, 130);"
                ]
                []
            , Svg.stop
                [ SvgA.offset "100%"
                , SvgA.style "stop-color: rgb(120, 100, 90);"
                ]
                []
            ]
        , Svg.filter
            [ SvgA.id "dropshadowFretboard"
            , SvgA.height "400%"
            ]
            [ Svg.feGaussianBlur
                [ SvgA.in_ "SourceAlpha"
                , SvgA.stdDeviation "3"
                ]
                []
            , Svg.feOffset
                [ SvgA.dx "4"
                , SvgA.dy "4"
                , SvgA.result "offsetblur"
                ]
                []
            , Svg.feComponentTransfer []
                [ Svg.feFuncA [ SvgA.slope "0.5" ] []
                ]
            , Svg.feMerge []
                [ Svg.feMergeNode [] []
                , Svg.feMergeNode
                    [ SvgA.in_ "SourceGraphic"
                    ]
                    []
                ]
            ]
        , Svg.filter
            [ SvgA.id "dropshadowFret"
            , SvgA.height "400%"
            ]
            [ Svg.feGaussianBlur
                [ SvgA.in_ "SourceAlpha"
                , SvgA.stdDeviation "3"
                ]
                []
            , Svg.feOffset
                [ SvgA.dx "2"
                , SvgA.dy "2"
                , SvgA.result "offsetblur"
                ]
                []
            , Svg.feComponentTransfer []
                [ Svg.feFuncA
                    [ SvgA.slope "0.3"
                    ]
                    []
                ]
            , Svg.feMerge [] [ Svg.feMergeNode [] [], Svg.feMergeNode [ SvgA.in_ "SourceGraphic" ] [] ]
            ]
        ]


outerPoly : Svg.Svg msg
outerPoly =
    Svg.polygon
        [ SvgA.points outerPolyPoints
        , SvgA.fill "url(#fretboard)"
        , SvgA.style "filter: url(#dropshadowFretboard);"
        , SvgA.strokeLinejoin "round"
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
        [ SvgA.d fretsPath
        , SvgA.strokeWidth "10"
        , SvgA.stroke "url(#fret)"
        , SvgA.style "filter: url(#dropshadowFret);"
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
            sin (radians (pi * (toFloat stringAmplitudeCycleDurationMS / toFloat (loopInt 0 stringAmplitudeCycleDurationMS timeElapsed)))) * stringMaxAmplitudePX * toFloat (noteDurationMS - timeElapsed) / toFloat noteDurationMS

        activeFretX_ =
            if timeElapsed < noteDurationMS then
                activeFretX voice

            else
                Instrument.width

        values_ =
            viewStringAnimationValues activeFretX_ (stringY index) Instrument.width (44000 / (voice.currentPitch * voice.currentPitch)) amplitude
    in
    Svg.animate [ SvgA.attributeName "d", SvgA.values (joinAnimationValues values_), SvgA.dur (String.fromInt stringAnimationDurationMS ++ "ms"), SvgA.repeatCount "indefinite" ] []


viewString : Instrument.Voice -> Int -> Int -> (MouseEvent.Model -> msg) -> Svg.Svg msg
viewString voice index time msgForMouseOver =
    Svg.path
        [ SvgA.cursor "crosshair"
        , SvgA.strokeWidth "2"
        , SvgA.stroke "rgba(255, 255, 255, 0.5)"
        , SvgA.fill "none"
        , onMouseOver msgForMouseOver
        , SvgA.id ("instrument-voice-" ++ String.fromInt index)
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
    Svg.text_ [ SvgA.y (String.fromFloat (stringY voiceIndex)), SvgA.x (String.fromFloat (Instrument.fretDistance noteIndex + 10.0)), SvgA.fill "rgba(255, 255, 255, 0.5)", SvgA.class "inert" ] [ Svg.text (String.fromFloat note) ]


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
    viewDebugNotes model ++ [ Svg.text_ [ SvgA.y "215" ] [ Svg.text ("playing notes " ++ viewDebugPlayingNotes model) ] ]


onMouseOver : (MouseEvent.Model -> msg) -> Svg.Attribute msg
onMouseOver event =
    Svg.Events.on "mouseover" (MouseEvent.mapDecode event)
