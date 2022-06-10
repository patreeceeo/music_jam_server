module MouseEvent exposing (Model, create, decode, mapDecode)

import Json.Decode as D

type alias Model =
    { offsetX : Int
    , offsetY : Int
    , buttons : Int
    }


create : Int -> Int -> Int -> Model
create offsetX offsetY buttons =
    { offsetX = offsetX
    , offsetY = offsetY
    , buttons = buttons
    }


decoder : D.Decoder Model
decoder =
    D.map3
        Model
        (D.field "offsetX" D.int)
        (D.field "offsetY" D.int)
        (D.field "buttons" D.int)

decode : D.Value -> Result D.Error Model
decode value =
  D.decodeValue decoder value

mapDecode : (Model -> msg) -> D.Decoder msg
mapDecode event =
  D.map event decoder
