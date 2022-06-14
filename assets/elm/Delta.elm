module Delta exposing (compose, Composer, map, Mapper)

type alias Getter model subModel = model -> subModel
type alias Updater msg model = msg -> model -> model
type alias Setter model subModel = subModel -> model -> model
type alias Composer msg model subModel = (Getter model subModel, Updater msg subModel, Setter model subModel)
type alias Mapper msg model subModel result = (Getter model subModel, (msg -> subModel -> result))


-- Like List.Extra.mapAccumr but simpler
mapAccumr : (a -> b -> a) -> a -> List b -> a
mapAccumr f acc0 list =
    List.foldr
        (\x acc1 ->
          f acc1 x
        )
        acc0
        list

compose : List (Composer msg model subModel) -> model -> msg -> model -> model
compose clist initModel msg currentModel =
  mapAccumr (runComposer msg currentModel) initModel clist

runComposer : msg -> model -> model -> (Composer msg model subModel) -> model
runComposer msg currentModel accModel (get, update, set) =
  set (update msg (get currentModel)) accModel

map : List (Mapper msg model subModel (Cmd msg)) -> msg -> model -> List (Cmd msg)
map mlist msg model =
  List.map (\mapper -> runMapper msg model mapper) mlist

runMapper : msg -> model -> Mapper msg model subModel result -> result
runMapper msg model (get, map_) =
  map_ msg (get model)
