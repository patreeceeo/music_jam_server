port module Instrument exposing (Model, Voice, createInstrument, createInstrumentVoice, setCurrentPitch, decodeInstrument, view, update, Msg, mouseDownString, createMouseEvent, sendMessage, PlaySoundCmd)

-- IN-HOUSE MODULES

import Utils exposing (joinPoints, joinNums)

-- STDLIB MODULES

import Array exposing (Array)
import Json.Decode as D
import Svg
import Svg.Attributes exposing (..)
import Svg.Events
import Html


-- PORTS

type alias PlaySoundCmd = { soundId : String
    , pitch : Float
    , volume : Float
    }

port sendMessage : PlaySoundCmd -> Cmd msg


-- MODEL

type alias Voice =
    { currentPitch : Float
    , currentVolume : Float
    , notes : List Float
    }


type alias Model =
    { voices : Array Voice
    }

createInstrument : List(Voice) -> Model
createInstrument voices =
  {
    voices = Array.fromList(voices)
  }

createInstrumentVoice : List(Float) -> Voice
createInstrumentVoice notes =
  {
    currentPitch = 0
    , currentVolume = 0
    , notes = notes
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
            (D.field "notes" (D.list D.float))
        )


-- UPDATE


type Msg = MouseDownString Int MouseEvent

mouseDownString : Int -> MouseEvent -> Msg
mouseDownString index event = MouseDownString index event

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDownString index event ->
          let
              pitch = (toFloat(event.offsetX) / instW * toFloat(fretCount))
          in
            ( setCurrentPitch model index pitch
            , sendMessage (PlaySoundCmd "acoustic-guitar" pitch 40)
            )


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


e : Float
e =
    2.718


k : Float
k =
    5.71584144995393e-2


fretDistance : Int -> Float
fretDistance index =
    instW * 1.3 * (1 - (e ^ (-k * toFloat index)))



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


inlays : List (Svg.Svg Msg)
inlays =
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
                [ dx "8"
                , dy "8"
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
                [ dx "4"
                , dy "4"
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
    }

createMouseEvent : Int -> Int -> MouseEvent
createMouseEvent offsetX offsetY =
  {
    offsetX = offsetX
    , offsetY = offsetY
    }

onMouseDownString : (MouseEvent -> msg) -> Svg.Attribute msg
onMouseDownString target =
  Svg.Events.on "mousedown" (D.map target decodeMouseDownStringEvent)

decodeMouseDownStringEvent : D.Decoder MouseEvent
decodeMouseDownStringEvent =
    D.map2
        MouseEvent
        (D.field "offsetX" D.int)
        (D.field "offsetY" D.int)

stringIndexes : List Int
stringIndexes =
    List.range 1 6


stringY : Int -> Float
stringY index =
    toFloat (index * 28)


stringPath : Int -> String
stringPath index =
    "M" ++ joinNums " " [ 0, stringY index, instW, stringY index ]


string : Int -> Svg.Svg Msg
string index =
    Svg.path
        [ d (stringPath index)
        , cursor "crosshair"
        , strokeWidth "2"
        , stroke "rgba(255, 255, 255, 0.5)"
        , onMouseDownString(MouseDownString(index))
        , id ("instrument-voice-" ++ String.fromInt(index))
        ]
        []


strings : List (Svg.Svg Msg)
strings =
    List.map
        (\index -> string index)
        stringIndexes

view : Model -> Int -> Html.Html Msg
view model time =
    Svg.svg
        [ class "instrument"
        , preserveAspectRatio "xMidYMid meet"
        , viewBox viewBox_
        ]
        ([ svgDefs
         , outerPoly
         , frets
         ]
            ++ inlays
            ++ strings
        )

