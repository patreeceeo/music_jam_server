module User.Interface exposing (seekDirectionForKey, SeekDirection(..), voiceIndexForKey, intensityOfKeyPress)

import KbdEvent

type SeekDirection = SeekForward | SeekBackward | NoSeek
seekDirectionForKey: KbdEvent.Key -> SeekDirection
seekDirectionForKey key =
  case key of
    KbdEvent.KeyLeft ->
      SeekBackward
    KbdEvent.KeyRight ->
      SeekForward
    KbdEvent.KeyUp ->
      SeekBackward
    KbdEvent.KeyDown ->
      SeekForward
    _ ->
      NoSeek


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
