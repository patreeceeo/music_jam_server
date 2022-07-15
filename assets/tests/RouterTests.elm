module RouterTests exposing (..)

import Browser
import CommonTypes exposing (Routes(..))
import Expect
import Message
import Router
import Test exposing (..)
import Url


suite : Test
suite =
    describe "Router"
        [ testUpdate ]


model : Router.Model
model =
    { key = Router.TestNavKey
    , history = []
    }


exampleUrlStringA : String
exampleUrlStringA =
    "http://example.com/lab/fretboard"


exampleUrlA : Maybe Url.Url
exampleUrlA =
    Url.fromString exampleUrlStringA


exampleUrlB : Maybe Url.Url
exampleUrlB =
    Url.fromString "http://example.com/lab/selectchord"


testUpdate : Test
testUpdate =
    case exampleUrlA of
        Just url ->
            describe "update"
                [ describe "UrlRequest"
                    ((\() ->
                        let
                            ( newModel, cmd ) =
                                Router.update_ (Message.UrlRequest (Browser.Internal url)) model
                        in
                        [ test "model" <|
                            \_ ->
                                Expect.equal model newModel
                        , test "command" <|
                            \_ ->
                                Expect.equal cmd (Router.PushUrl Router.TestNavKey exampleUrlStringA)
                        ]
                     )
                        ()
                    )
                , describe "UrlChange"
                    ((\() ->
                        let
                            ( newModel, cmd ) =
                                Router.update_ (Message.UrlChange url) model
                        in
                        [ test "model" <|
                            \_ ->
                                Expect.equal { model | history = [ MainRoute ] } newModel
                        , test "command" <|
                            \_ ->
                                Expect.equal cmd Router.NoCmd
                        ]
                     )
                        ()
                    )
                , describe "RequestPreviousUrl"
                    ((\() ->
                        let
                            ( newModel, cmd ) =
                                Router.update_ (Message.RequestPreviousUrl 1) model
                        in
                        [ test "model" <|
                            \_ ->
                                Expect.equal model newModel
                        , test "command" <|
                            \_ ->
                                Expect.equal cmd Router.NoCmd
                        ]
                     )
                        ()
                    )
                , describe "RequestPreviousUrl when there's history"
                    ((\() ->
                        let
                            newModel = { model | history = [ MainRoute, MainRoute ] }
                            ( newerModel, cmd ) =
                                Router.update_ (Message.RequestPreviousUrl 2) newModel
                        in
                        [ test "model" <|
                            \_ ->
                                Expect.equal model newerModel
                        , test "command" <|
                            \_ ->
                                Expect.equal cmd (Router.GoBack Router.TestNavKey 2)
                        ]
                     )
                        ()
                    )
                ]

        Nothing ->
            test "" <| \_ -> Expect.fail "exampleUrlA isn't a url"
