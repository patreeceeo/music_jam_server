module Flags exposing (Model, decoder, tryDecodingFor)

import Browser.Navigation
import CommonTypes exposing (ClientId, Routes(..))
import Instrument
import Json.Decode as D
import Url exposing (Url)


type alias Model =
    { clientId : ClientId
    , screenWidth : Int
    , baseHref : String
    , instrument : Instrument.Model
    , nextRoute : Routes
    }


decoder : D.Decoder Model
decoder =
    D.map5 Model
        (D.field "clientId" D.string)
        (D.field "screenWidth" D.int)
        (D.field "baseHref" D.string)
        -- Just decode list of voices instead of complete instrument model
        (D.field "instrument" Instrument.decoder)
        (D.succeed SelectChordRoute)


tryDecodingFor : (Result D.Error Model -> Url -> Browser.Navigation.Key -> initResult) -> D.Value -> Url -> Browser.Navigation.Key -> initResult
tryDecodingFor fn encodedFlags =
    fn (D.decodeValue decoder encodedFlags)
