module Modely exposing (Composer, compose)

import Utils


type alias Getter model subModel =
    model -> subModel


type alias Updater msg model selectors return =
    msg -> model -> selectors -> return


type alias Setter model subModel =
    subModel -> model -> model


type alias Composer msg model subModel selectors updateReturn =
    ( Getter model subModel, Updater msg subModel selectors updateReturn, Setter model subModel )


compose : List (Composer msg model subModel selectors ( subModel, cmd )) -> (List cmd -> cmd) -> (model -> selectors) -> msg -> model -> ( model, cmd )
compose clist batchCmds selectors msg model =
    let
        ( newModel, cmdList ) =
            Utils.mapAccumr (runComposer msg model (selectors model)) ( model, [] ) clist
    in
    ( newModel, batchCmds cmdList )


runComposer : msg -> model -> selectors -> ( model, List cmd ) -> Composer msg model subModel selectors ( subModel, cmd ) -> ( model, List cmd )
runComposer msg currentModel selectors acc ( get, update, set ) =
    let
        ( accModel, accCmd ) =
            acc

        ( newModel, newCmd ) =
            update msg (get currentModel) selectors
    in
    ( set newModel accModel, List.append [ newCmd ] accCmd )
