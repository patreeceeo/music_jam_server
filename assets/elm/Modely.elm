module Modely exposing (Composer, compose)

-- TODO rename to Modely


type alias Getter model subModel =
    model -> subModel


type alias Updater msg model return =
    msg -> model -> return


type alias Setter model subModel =
    subModel -> model -> model


type alias Composer msg model subModel updateReturn =
    ( Getter model subModel, Updater msg subModel updateReturn, Setter model subModel )



-- Like List.Extra.mapAccumr but simpler


mapAccumr : (a -> b -> a) -> a -> List b -> a
mapAccumr f acc0 list =
    List.foldr
        (\x acc1 ->
            f acc1 x
        )
        acc0
        list


compose : List (Composer msg model subModel ( subModel, cmd )) -> (List cmd -> cmd) -> msg -> model -> ( model, cmd )
compose clist batchCmds msg model =
    let
        ( newModel, cmdList ) =
            mapAccumr (runComposer msg model) ( model, [] ) clist
    in
    ( newModel, batchCmds cmdList )


runComposer : msg -> model -> ( model, List cmd ) -> Composer msg model subModel ( subModel, cmd ) -> ( model, List cmd )
runComposer msg currentModel acc ( get, update, set ) =
    let
        ( accModel, accCmd ) =
            acc

        ( newModel, newCmd ) =
            update msg (get currentModel)
    in
    ( set newModel accModel, List.append [ newCmd ] accCmd )
