port module PortMessage exposing (Message(..), RawMessage, decode, encode, receive, send)

import Json.Decode as D
import Json.Encode as E


type alias RawMessage =
    E.Value


type alias MessageRecord =
    { type_ : String, data : PlayNoteRecord }


type alias PlayNoteRecord =
    { soundId : String, voiceIndex : Int, pitch : Float, volume : Float }


type Message
    = PlayNote PlayNoteRecord
    | LogError String
    | AppStateChange Bool


port outbox : E.Value -> Cmd msg


port inbox : (E.Value -> msg) -> Sub msg


send : Message -> Cmd mdg
send msg =
    outbox (encode msg)


receive : (E.Value -> msg) -> Sub msg
receive receiver =
    inbox receiver


encode : Message -> E.Value
encode msg =
    case msg of
        PlayNote data ->
            E.object
                [ ( "type", E.string "playNote" )
                , ( "data"
                  , E.object
                        [ ( "soundId", E.string data.soundId )
                        , ( "voiceIndex", E.int data.voiceIndex )
                        , ( "pitch", E.float data.pitch )
                        , ( "volume", E.float data.volume )
                        ]
                  )
                ]

        LogError errMsg ->
            E.object
                [ ( "type", E.string "logError" )
                , ( "data"
                  , E.object
                        [ ( "message", E.string errMsg ) ]
                  )
                ]

        AppStateChange sleeping ->
            E.object
                [ ( "type", E.string "appStateChange" )
                , ( "data"
                  , E.object
                        [ ( "sleeping", E.bool sleeping ) ]
                  )
                ]


decode : RawMessage -> Result D.Error MessageRecord
decode raw =
    D.decodeValue decoder raw


decoder : D.Decoder MessageRecord
decoder =
    D.at [ "type" ] D.string
        |> D.andThen decoder_byType


decoder_byType : String -> D.Decoder MessageRecord
decoder_byType type_ =
    case type_ of
        "playNote" ->
            decoder_playNote

        _ ->
            D.fail ("unhandled message type " ++ type_)


decoder_playNote : D.Decoder MessageRecord
decoder_playNote =
    D.map2 MessageRecord
        (D.field "type" D.string)
        (D.field "data" decoder_playNoteData)


decoder_playNoteData : D.Decoder PlayNoteRecord
decoder_playNoteData =
    D.map4 PlayNoteRecord
        (D.field "soundId" D.string)
        (D.field "voiceIndex" D.int)
        (D.field "pitch" D.float)
        (D.field "volume" D.float)
