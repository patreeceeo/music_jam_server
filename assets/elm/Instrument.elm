module Instrument exposing (Model, Voice, chordNotes, currentPitch, decoder, fretCount, fretDistance, fretIndex, fretWidth, height, init, initVoice, isInlayFret, pitchAtOffset, setActiveChord, setCurrentPitch, setCurrentVolume, setLastNoteStartTime, update, width)

import Array exposing (Array)
import Chord
import CommonTypes exposing (Pitch, QTime, Selectors, Volume)
import Json.Decode as D
import List.Extra
import Maybe.Extra
import Message exposing (Message)
import PortMessage
import Svg.Attributes exposing (..)
import Utils



-- TODO DRY up and add unit tests
-- CONSTANTS


mouseOverVoiceVolume : Volume
mouseOverVoiceVolume =
    0.5



-- DECODE JSON


decoder : D.Decoder Model
decoder =
    D.map Model
        (D.field "voices" decodeInstrumentVoices)


decodeInstrumentVoices : D.Decoder (Array Voice)
decodeInstrumentVoices =
    D.array
        (D.map4 Voice
            (D.field "currentPitch" (D.maybe D.float))
            (D.field "currentVolume" D.float)
            (D.field "lastNoteStartTime" D.int)
            (D.field "notes" (D.array D.float))
        )



-- MODEL


type alias Voice =
    { currentPitch : Maybe Pitch -- The pitch of the note that would be played when currentVolume > 0, Nothing implies voice is muted
    , currentVolume : Volume
    , lastNoteStartTime : QTime

    {- Indexed according to the physical layout of the instrument -}
    , notes : Array Pitch
    }



{- Musical chord. For each voice, this says whether that voice participates in the chord and in which pitch.
   E.g. Am = [ (Just 65), (Just 61), (Just 57), (Just 50), Nothing, Nothing ]
-}


type alias Model =
    { voices : Array Voice
    }


init : List Voice -> Model
init voices =
    { voices = Array.fromList voices
    }


initVoice : List Float -> Voice
initVoice notes =
    { currentPitch = Nothing
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
    { voice | currentPitch = Just pitch }


asCurrentVolumeIn : Voice -> Float -> Voice
asCurrentVolumeIn voice pitch =
    { voice | currentVolume = pitch }


asLastNoteStartTimeIn : Voice -> Int -> Voice
asLastNoteStartTimeIn voice when =
    { voice | lastNoteStartTime = when }


setCurrentPitch : Float -> Int -> Model -> Model
setCurrentPitch pitch voiceIndex instrument =
    voiceAt voiceIndex instrument
        |> Maybe.Extra.unwrap instrument
            (\voice ->
                pitch
                    |> asCurrentPitchIn voice
                    |> asVoiceIn voiceIndex instrument
            )


currentPitch : Int -> Model -> Maybe Pitch
currentPitch voiceIndex instrument =
    case voiceAt voiceIndex instrument of
        Just voice ->
            voice.currentPitch

        _ ->
            Nothing


setCurrentVolume : Float -> Int -> Model -> Model
setCurrentVolume volume voiceIndex instrument =
    voiceAt voiceIndex instrument
        |> Maybe.Extra.unwrap instrument
            (\voice ->
                volume
                    |> asCurrentVolumeIn voice
                    |> asVoiceIn voiceIndex instrument
            )


setLastNoteStartTime : Int -> Int -> Model -> Model
setLastNoteStartTime when voiceIndex instrument =
    voiceAt voiceIndex instrument
        |> Maybe.Extra.unwrap instrument
            (\voice ->
                when
                    |> asLastNoteStartTimeIn voice
                    |> asVoiceIn voiceIndex instrument
            )


setActiveChord : Chord.Chord -> Model -> Model
setActiveChord chord model =
    let
        mappedChord =
            List.indexedMap
                (\voiceIndex comp ->
                    comp
                        |> Maybe.andThen
                            (\noteIndex ->
                                noteAt noteIndex voiceIndex model
                            )
                )
                chord
    in
    List.Extra.indexedFoldr setChordComponent model mappedChord


playNote : Int -> Float -> Float -> Int -> Model -> Model
playNote voiceIndex pitch volume when instrument =
    instrument
        |> setCurrentPitch pitch voiceIndex
        |> setLastNoteStartTime when voiceIndex
        |> setCurrentVolume volume voiceIndex


setChordComponent : Int -> Maybe Float -> Model -> Model
setChordComponent voiceIndex maybePitch model =
    maybePitch
        |> Maybe.Extra.unwrap model
            (\pitch ->
                model
                    |> setCurrentPitch pitch voiceIndex
            )


chordNotes : Volume -> Model -> List PortMessage.PlaySoundRecord
chordNotes volume model =
    Array.toList model.voices
        |> List.indexedMap (\index voice -> voice.currentPitch |> Maybe.map (\pitch -> PortMessage.PlaySoundRecord "acoustic-guitar" index pitch volume))
        |> List.filterMap identity



-- UPDATE


update : Message -> Model -> Selectors -> ( Model, Cmd Message )
update msg instrument select =
    let
        timeInMillis =
            select.timeInMillis ()
    in
    case msg of
        Message.PlayChord volume ->
            let
                notes =
                    chordNotes volume instrument
            in
            ( Utils.mapAccumr (\acc note -> playNote note.voiceIndex note.pitch note.volume timeInMillis acc) instrument notes
            , Cmd.batch (List.Extra.reverseMap (\playSound -> PortMessage.send (PortMessage.PlaySound playSound)) notes)
            )

        Message.PlayNote volume voiceIndex pitch ->
            ( playNote voiceIndex pitch volume timeInMillis instrument
            , PortMessage.send (PortMessage.PlaySound { soundId = "acoustic-guitar", voiceIndex = voiceIndex, pitch = pitch, volume = volume })
            )

        -- TODO replace MouseOverVoice with PlayNote
        Message.MouseOverVoice index event ->
            let
                screenWidth =
                    select.screenWidth ()
            in
            if event.buttons > 0 then
                let
                    pitchResult =
                        pitchAtOffset event.offsetX screenWidth instrument index
                in
                case pitchResult of
                    Ok pitch ->
                        ( playNote index pitch mouseOverVoiceVolume timeInMillis instrument
                        , PortMessage.send (PortMessage.PlaySound { soundId = "acoustic-guitar", voiceIndex = index, pitch = pitch, volume = mouseOverVoiceVolume })
                        )

                    Err errMsg ->
                        ( instrument, PortMessage.send (PortMessage.LogError errMsg) )

            else
                ( instrument, Cmd.none )

        Message.SelectChord chordName ->
            let
                chord =
                    Chord.forName chordName
            in
            if chordName /= Chord.X_Chord then
                ( setActiveChord chord instrument, Cmd.none )

            else
                ( instrument, Cmd.none )

        Message.ReceivePortMessage _ rawMsg ->
            case PortMessage.decode rawMsg of
                Ok playSound ->
                    case playSound.data of
                        PortMessage.PlaySound data ->
                            ( playNote data.voiceIndex data.pitch data.volume timeInMillis instrument
                            , Cmd.none
                            )

                        _ ->
                            ( instrument, Cmd.none )

                Err error ->
                    ( instrument
                    , PortMessage.send (PortMessage.LogError (D.errorToString error))
                    )

        _ ->
            ( instrument, Cmd.none )



-- handlePlayChord : Volume -> QTime -> Model -> ( Model, Cmd Message )
-- handlePlayChord volume when instrument =
--     let
--         mChord =
--             mapActiveChord instrument
--         messages =
--             messagesForMappedChord mChord volume
--     in
--     ( playMappedChord mChord volume when instrument
--     , Cmd.batch (List.Extra.reverseMap (Maybe.Extra.unwrap Cmd.none (\playSound -> PortMessage.send (PortMessage.PlaySound playSound))) messages)
--     )


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



-- HELPERS


voiceAt : Int -> Model -> Maybe Voice
voiceAt voiceIndex model =
    Array.get voiceIndex model.voices


noteAt : Int -> Int -> Model -> Maybe Float
noteAt noteIndex voiceIndex model =
    voiceAt voiceIndex model
        |> Maybe.andThen (\voice -> Array.get noteIndex voice.notes)
