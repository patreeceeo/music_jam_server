module ModelyTests exposing (..)

import Modely
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Modely"
        [ describe "compose" testCompose ]


type Msg
    = Inc
    | Foo


batchCmds : List String -> String
batchCmds list =
    String.join ", " list


testCompose : List Test
testCompose =
    let
        model =
            { subA = { count = 0 }
            , subB = { count = 0 }
            }

        subUpdate =
            \msg subModel ->
                if msg == Inc then
                    ( { subModel | count = subModel.count + 1 }, "cmd a" )

                else
                    ( subModel, "cmd b" )

        subApps =
            [ ( \model_ -> model_.subA
              , subUpdate
              , \subModel model_ -> { model_ | subA = subModel }
              )
            , ( \model_ -> model_.subB
              , subUpdate
              , \subModel model_ -> { model_ | subB = subModel }
              )
            ]

        updateFn =
            Modely.compose subApps batchCmds

        ( newModel1, cmd ) =
            updateFn Inc model

        ( newModel2, _ ) =
            updateFn Foo newModel1
    in
    [ test "update model" <|
        \_ ->
            Expect.equal { subA = { count = 1 }, subB = { count = 1 } } newModel2
    , test "batch commands" <|
        \_ ->
            Expect.equal "cmd a, cmd a" cmd
    ]
