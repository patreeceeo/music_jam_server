module Instrument exposing (Model, Voice, init, initVoice, decoder, fretCount, fretDistance, fretIndex, fretWidth, height, width, isInlayFret, pitchAtOffset, playNote, setCurrentPitch)

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


init : List Voice -> Model
init voices =
    { voices = Array.fromList voices
    }


initVoice : List Float -> Voice
initVoice notes =
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
        |> flip3 setCurrentPitch pitch voiceIndex
        |> flip3 setLastNoteStartTime when voiceIndex



-- DECODE JSON


decoder : D.Decoder Model
decoder =
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
            (toFloat offset / toFloat screenWidth) * width

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




-- TODO turn some of these into parameters
width : Float
width =
    2000


height : Float
height =
    200


fretCount : Int
fretCount =
    24


k : Float
k =
    5.71584144995393e-2


fretDistance : Int -> Float
fretDistance index =
    width * 1.3 * (1 - (e ^ (-k * toFloat index)))


fretIndex : Float -> Int
fretIndex distance =
    let
        antiLog =
            -1 * (distance - (width * 1.3)) / (width * 1.3)
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
