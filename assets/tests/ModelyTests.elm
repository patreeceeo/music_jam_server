module ModelyTests exposing (..)

import Expect
import Modely
import Test exposing (..)
import Dict


suite : Test
suite =
    describe "Modely"
        [ describe "compose" testCompose ]

type alias Model = { subA: SubModel, subB : SubModel }
type alias SubModel = { count: Int }

type Msg
    = Inc
    | Foo

type SelectorParams =
  APlusParams Int

type SelectorReturn =
  APlusReturn Int

batchCmds : List String -> String
batchCmds list =
    String.join ", " list


subUpdate : Msg -> SubModel -> Modely.SelectorsByName SelectorParams SelectorReturn -> (SubModel, String)
subUpdate msg subModel selectors =
    if msg == Inc then
      let
          (APlusReturn aPlusReturn) = Modely.select selectors "aPlus" (APlusParams 5) (APlusReturn 0)
      in
      ( { subModel | count = subModel.count + 1 + aPlusReturn }, "cmd a" )

    else
      ( subModel, "cmd b" )

bindSelectors : Model -> Modely.SelectorsByName SelectorParams SelectorReturn
bindSelectors model = Dict.fromList [ ("aPlus", \(APlusParams amount) -> APlusReturn (model.subA.count + amount))]

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
