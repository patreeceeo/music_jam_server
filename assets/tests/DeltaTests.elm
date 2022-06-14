module DeltaTests exposing (..)

import Delta
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Delta module"
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
                    ({ subModel | count = subModel.count + 1 }, "cmd a")

                else
                    (subModel, "cmd b")

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
            Delta.compose subApps batchCmds

        (newModel1, cmd) =
            updateFn Inc model
        (newModel2, _) =
            updateFn Foo newModel1
    in
    [ test "update model" <|
        \_ ->
            Expect.equal { subA = { count = 1 }, subB = { count = 1 } } newModel2
    , test "batch commands" <|
        \_ ->
            Expect.equal "cmd a, cmd a" cmd

    -- , test "map" <|
    --     \_ ->
    --         let
    --             flist =
    --                 [ ( \model_ -> model_.subA
    --                   , \_ _ -> Cmd.none
    --                   )
    --                 , ( \model_ -> model_.subB
    --                   , \_ _ -> Cmd.none
    --                   )
    --                 ]

    --             mapped =
    --                 Delta.map flist Foo model
    --         in
    --         Expect.equal [ Cmd.none, Cmd.none ] mapped
    ]
