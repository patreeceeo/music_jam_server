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


testCompose : List Test
testCompose =
    let
        model =
            { subA = { count = 0 }
            , subB = { count = 0 }
            }
    in
    [ test "update model" <|
        \_ ->
            let
                subUpdate =
                    \msg subModel ->
                        if msg == Inc then
                            { subModel | count = subModel.count + 1 }

                        else
                            subModel

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
                    Delta.compose subApps model

                newModel =
                    updateFn Foo (updateFn Inc model)
            in
            Expect.equal { subA = { count = 1 }, subB = { count = 1 } } newModel
    , test "map" <|
        \_ ->
            let
                flist =
                    [ ( \model_ -> model_.subA
                      , \_ _ -> Cmd.none
                      )
                    , ( \model_ -> model_.subB
                      , \_ _ -> Cmd.none
                      )
                    ]

                mapped =
                    Delta.map flist Foo model
            in
          Expect.equal [ Cmd.none, Cmd.none ] mapped
  ]
