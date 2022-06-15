module Modely exposing (Composer, SelectorsByName, compose)

type alias Getter model subModel =
    model -> subModel


type alias Updater msg model selectors return =
    msg -> model -> selectors -> return


type alias Setter model subModel =
    subModel -> model -> model


type alias Composer msg model subModel selectors updateReturn =
    ( Getter model subModel, Updater msg subModel selectors updateReturn, Setter model subModel )


type alias SelectorsByName selectorParams selectorReturn =
    (selectorParams -> selectorReturn)



-- Like List.Extra.mapAccumr but simpler


mapAccumr : (a -> b -> a) -> a -> List b -> a
mapAccumr f acc0 list =
    List.foldr
        (\x acc1 ->
            f acc1 x
        )
        acc0
        list


compose : List (Composer msg model subModel (SelectorsByName selectorParams selectorReturn) ( subModel, cmd )) -> (List cmd -> cmd) -> SelectorsByName selectorParams selectorReturn -> msg -> model -> ( model, cmd )
compose clist batchCmds selectors msg model =
    let
        ( newModel, cmdList ) =
            mapAccumr (runComposer msg model selectors) ( model, [] ) clist
    in
    ( newModel, batchCmds cmdList )


runComposer : msg -> model -> SelectorsByName selectorParams selectorReturn -> ( model, List cmd ) -> Composer msg model subModel (SelectorsByName selectorParams selectorReturn) ( subModel, cmd ) -> ( model, List cmd )
runComposer msg currentModel selectors acc ( get, update, set ) =
    let
        ( accModel, accCmd ) =
            acc

        ( newModel, newCmd ) =
            update msg (get currentModel) selectors
    in
    ( set newModel accModel, List.append [ newCmd ] accCmd )
