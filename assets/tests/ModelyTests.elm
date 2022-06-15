module ModelyTests exposing (..)

import Expect
import Modely
import Test exposing (..)


suite : Test
suite =
    describe "Modely"
        [ describe "compose" testCompose ]


type alias Model =
    { subA : SubModel, subB : SubModel }


type alias SubModel =
    { count : Int }


type Msg
    = Inc
    | Foo


type Selectors
    = APlus Int


type SelectorReturn
    = APlusReturn Int


batchCmds : List String -> String
batchCmds list =
    String.join ", " list


subUpdate : Msg -> SubModel -> Modely.SelectorsByName Selectors SelectorReturn -> ( SubModel, String )
subUpdate msg subModel selectors =
    if msg == Inc then
        let
            (APlusReturn aPlusReturn) =
                selectors (APlus 5)
        in
        ( { subModel | count = subModel.count + 1 + aPlusReturn }, "cmd a" )

    else
        ( subModel, "cmd b" )


bindSelectors : Model -> Modely.SelectorsByName Selectors SelectorReturn
bindSelectors model call = 
 case call of
   APlus amount -> APlusReturn (model.subA.count + amount)



testCompose : List Test
testCompose =
    let
        model =
            { subA = { count = 4 }
            , subB = { count = 0 }
            }

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
            Modely.compose subApps batchCmds (bindSelectors model)

        ( newModel1, cmd ) =
            updateFn Inc model

        ( newModel2, _ ) =
            updateFn Foo newModel1
    in
    [ test "update model" <|
        \_ ->
            Expect.equal { subA = { count = 14 }, subB = { count = 10 } } newModel2
    , test "batch commands" <|
        \_ ->
            Expect.equal "cmd a, cmd a" cmd
    ]
