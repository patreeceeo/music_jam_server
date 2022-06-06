module MouseEvent exposing (Model, create, decode)

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


decode : D.Decoder Model
decode =
    D.map3
        Model
        (D.field "offsetX" D.int)
        (D.field "offsetY" D.int)
        (D.field "buttons" D.int)

