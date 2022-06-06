module Instrument exposing (Model, Voice, createInstrument, createInstrumentVoice, decodeInstrument, fretDistance, fretIndex, pitchAtOffset, setCurrentPitch, playNote, instW, instH, fretWidth, fretCount, isInlayFret)

import Array exposing (Array)
import Json.Decode as D
import Svg.Attributes exposing (..)
import Utils exposing (flip3)


-- MODEL


type alias Voice =
    { currentPitch : Float
    , currentVolume : Float
    , lastNoteStartTime : Int
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
    , lastNoteStartTime = 0
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

asLastNoteStartTimeIn : Voice -> Int -> Voice
asLastNoteStartTimeIn voice when =
    { voice | lastNoteStartTime = when }


setCurrentPitch : Model -> Int -> Float -> Model
setCurrentPitch instrument voiceIndex pitch =
    case Array.get voiceIndex instrument.voices of
        Just voice ->
            pitch
                |> asCurrentPitchIn voice
                |> asVoiceIn voiceIndex instrument

        Nothing ->
            instrument

setLastNoteStartTime : Model -> Int -> Int -> Model
setLastNoteStartTime instrument voiceIndex when =
  case Array.get voiceIndex instrument.voices of
    Just voice ->
      when
        |> asLastNoteStartTimeIn voice
        |> asVoiceIn voiceIndex instrument
    Nothing ->
      instrument

playNote : Model -> Int -> Float -> Float -> Int -> Model
playNote instrument voiceIndex pitch volume when =
  instrument
    |> ((flip3 setCurrentPitch) pitch voiceIndex)
    |> ((flip3 setLastNoteStartTime) when voiceIndex)

-- DECODE JSON


decodeInstrument : D.Decoder Model
decodeInstrument =
    D.map Model
        (D.field "voices" decodeInstrumentVoices)


decodeInstrumentVoices : D.Decoder (Array Voice)
decodeInstrumentVoices =
    D.array
        (D.map4 Voice
            (D.field "currentPitch" D.float)
            (D.field "currentVolume" D.float)
            (D.field "lastNoteStartTime" D.int)
            (D.field "notes" (D.array D.float))
        )



-- UPDATE


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
                    Err ("OutOfRangeErr: no note corresponding to offset " ++ String.fromInt offset ++ " (note index: " ++ String.fromInt noteIndex ++ " on voice index " ++ String.fromInt voiceIndex)

        Nothing ->
            Err ("OutOfRangeErr: no voice corresponding to index " ++ String.fromInt voiceIndex)



-- VIEW


instW : Float
instW =
    2000


instH : Float
instH =
    200

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
    ceiling ((-1 * logBase e antiLog / k) - 1)



-- TODO probably a more efficient implementation, isn't there?


fretWidth : Int -> Float
fretWidth index =
    fretDistance (index + 1) - fretDistance index


isEqualRemainder : Int -> Int -> Int -> Bool
isEqualRemainder dividend divisor remainder =
    remainder == modBy divisor dividend


isInlayFret : Int -> Bool
isInlayFret index =
    List.any (isEqualRemainder index 12) [ 3, 5, 7, 9, 0 ]


