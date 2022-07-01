module FormElementEvent exposing (decodeValue)

import Html.Events
import Json.Decode


decodeValue : (String -> msg) -> Json.Decode.Decoder msg
decodeValue tagger =
    Json.Decode.map tagger Html.Events.targetValue
