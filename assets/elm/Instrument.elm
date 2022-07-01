module Instrument exposing (Model, Voice, decoder, fretCount, fretDistance, fretIndex, fretWidth, height, init, initVoice, isInlayFret, mapActiveChord, messagesForMappedChord, pitchAtOffset, setActiveChord, setCurrentPitch, setCurrentVolume, setLastNoteStartTime, update, width)

import Array exposing (Array)
import Chord
import CommonTypes exposing (NoteVIndex, Pitch, QTime, Volume)
import Json.Decode as D
import KbdEvent
import List.Extra
import Maybe.Extra
import Message exposing (Message)
import PortMessage
import Selectors
import Svg.Attributes exposing (..)



-- TODO DRY up and add unit tests
-- CONSTANTS


mouseOverVoiceVolume : Volume
mouseOverVoiceVolume =
    0.5



-- DECODE JSON


decoder : D.Decoder Model
decoder =
    D.map2 Model
        (D.field "voices" decodeInstrumentVoices)
        (D.succeed Chord.none)


decodeInstrumentVoices : D.Decoder (Array Voice)
decodeInstrumentVoices =
    D.array
        (D.map4 Voice
            (D.field "currentPitch" D.float)
            (D.field "currentVolume" D.float)
            (D.field "lastNoteStartTime" D.int)
            (D.field "notes" (D.array D.float))
        )



-- MODEL


type alias Voice =
    { currentPitch : Pitch
    , currentVolume : Volume
    , lastNoteStartTime : QTime

    {- Indexed according to the physical layout of the instrument -}
    , notes : Array Pitch
    }



{- Musical chord. For each voice, this says whether that voice participates in the chord and in which pitch.
   E.g. Am = [ (Just 65), (Just 61), (Just 57), (Just 50), Nothing, Nothing ]
-}


type alias MappedChord =
    List (Maybe Pitch)


type alias Model =
    { voices : Array Voice
    , activeChord : Chord.Chord
    }


init : List Voice -> Model
init voices =
    { voices = Array.fromList voices
    , activeChord = Chord.none
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
    { model | activeChord = chord }


playNote : Int -> Float -> Float -> Int -> Model -> Model
playNote voiceIndex pitch volume when instrument =
    instrument
        |> setCurrentPitch pitch voiceIndex
        |> setLastNoteStartTime when voiceIndex
        |> setCurrentVolume volume voiceIndex



{- map a chord to its list of component pitches -}


mapActiveChord : Model -> List (Maybe Float)
mapActiveChord model =
    List.indexedMap
        (\voiceIndex comp ->
            comp
                |> Maybe.andThen
                    (\noteIndex ->
                        noteAt noteIndex voiceIndex model
                    )
        )
        model.activeChord


playChordComponent : Float -> Int -> Int -> Maybe Float -> Model -> Model
playChordComponent volume when voiceIndex maybePitch instrument =
    maybePitch
        |> Maybe.Extra.unwrap instrument
            (\pitch ->
                playNote voiceIndex pitch volume when instrument
            )



-- TODO create type aliases for notes pitches etc


playMappedChord : MappedChord -> Float -> Int -> Model -> Model
playMappedChord chord volume when instrument =
    List.Extra.indexedFoldr (playChordComponent volume when) instrument chord


messagesForMappedChord : MappedChord -> Volume -> List (Maybe PortMessage.PlaySoundRecord)
messagesForMappedChord chord volume =
    List.indexedMap (\index maybePitch -> maybePitch |> Maybe.map (\pitch -> { soundId = "acoustic-guitar", voiceIndex = index, pitch = pitch, volume = volume })) chord



-- UPDATE


update : Message -> Model -> Selectors.Selectors -> ( Model, Cmd Message )
update msg instrument select =
    let
        timeInMillis =
            select.timeInMillis ()
    in
    case msg of
        Message.PlayChord volume ->
            handlePlayChord volume timeInMillis instrument

        Message.KeyUp event ->
            let
                milisSinceKeyDown =
                    select.milisSinceKeyDown event.key

                volume =
                    intensityOfKeyPress milisSinceKeyDown
            in
            case event.key of
                KbdEvent.KeySpace ->
                    handlePlayChord volume timeInMillis instrument

                _ ->
                    let
                        default =
                            ( instrument, Cmd.none )
                    in
                    voiceIndexForKey event.key
                        |> Maybe.Extra.unwrap default
                            (\voiceIndex ->
                                noteAt 0 voiceIndex instrument
                                    |> Maybe.Extra.unwrap default
                                        (\pitch ->
                                            ( playNote voiceIndex pitch volume timeInMillis instrument
                                            , PortMessage.send (PortMessage.PlaySound { soundId = "acoustic-guitar", voiceIndex = voiceIndex, pitch = pitch, volume = volume })
                                            )
                                        )
                            )

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
            if Debug.log "SelectChord" chordName /= Chord.X_Chord then
                ( setActiveChord chord instrument, Cmd.none )

            else
                ( instrument, Cmd.none )

        Message.ReceivePortMessage rawMsg ->
            case PortMessage.decode rawMsg of
                Ok playSound ->
                    case Debug.log "PlaySound data" playSound.data of
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


handlePlayChord : Volume -> QTime -> Model -> ( Model, Cmd Message )
handlePlayChord volume when instrument =
    let
        mChord =
            mapActiveChord instrument

        messages =
            messagesForMappedChord mChord volume
    in
    ( playMappedChord mChord volume when instrument
    , Cmd.batch (List.Extra.reverseMap (Maybe.Extra.unwrap Cmd.none (\playSound -> PortMessage.send (PortMessage.PlaySound playSound))) messages)
    )


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


voiceIndexForKey : KbdEvent.Key -> Maybe Int
voiceIndexForKey key =
    case key of
        KbdEvent.KeyS ->
            Just 0

        KbdEvent.KeyD ->
            Just 1

        KbdEvent.KeyF ->
            Just 2

        KbdEvent.KeyJ ->
            Just 3

        KbdEvent.KeyK ->
            Just 4

        KbdEvent.KeyL ->
            Just 5

        _ ->
            Nothing


intensityOfKeyPress : Int -> Float
intensityOfKeyPress milisSinceKeyDown =
    Basics.min 100 (toFloat milisSinceKeyDown / 100)
