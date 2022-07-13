module User.Interface exposing (UserAction(..), intensityOfKeyPress, userActionForKey, voiceIndexForKey)

import KbdEvent


type UserAction
    = SeekForward
    | SeekBackward
    | Dismiss
    | Open
    | NoAction


userActionForKey : KbdEvent.Key -> UserAction
userActionForKey key =
    case key of
        KbdEvent.KeyJ ->
            SeekForward

        KbdEvent.KeyK ->
            SeekForward

        KbdEvent.KeyLeft ->
            SeekBackward

        KbdEvent.KeyRight ->
            SeekForward

        KbdEvent.KeyUp ->
            SeekBackward

        KbdEvent.KeyDown ->
            SeekForward

        KbdEvent.KeyEsc ->
            Dismiss

        KbdEvent.KeyEnter ->
            Open

        _ ->
            NoAction


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
