module KbdEvent exposing (Key(..), Model, create, decode)

import Json.Decode as D


type Key
    = KeyA
    | KeyS
    | KeyD
    | KeyF
    | KeyG
    | KeyH
    | KeyJ
    | KeyK
    | KeyL
    | KeySpace
    | KeyLeft
    | KeyRight
    | KeyUp
    | KeyDown


type alias Model =
    { key : Key
    }


create : Key -> Model
create key =
    { key = key }


decode : D.Decoder Model
decode =
    D.map
        Model
        (D.field "code" decodeKey)


decodeKey : D.Decoder Key
decodeKey =
    D.string |> D.andThen keyFromString


keyFromString : String -> D.Decoder Key
keyFromString str =
    case str of
        "KeyA" ->
            D.succeed KeyA

        "KeyS" ->
            D.succeed KeyS

        "KeyD" ->
            D.succeed KeyD

        "KeyF" ->
            D.succeed KeyF

        "KeyG" ->
            D.succeed KeyG

        "KeyH" ->
            D.succeed KeyH

        "KeyJ" ->
            D.succeed KeyJ

        "KeyK" ->
            D.succeed KeyK

        "KeyL" ->
            D.succeed KeyL

        "Space" ->
            D.succeed KeySpace

        "ArrowRight" ->
            D.succeed KeyRight

        "ArrowLeft" ->
            D.succeed KeyLeft

        "ArrowUp" ->
            D.succeed KeyUp

        "ArrowDown" ->
            D.succeed KeyDown

        _ ->
            D.fail (Debug.log "" ("invalid key code " ++ str))
