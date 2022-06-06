module Utils exposing (joinNums, joinPoints, flip3)


joinNums : String -> List Float -> String
joinNums separator nums =
    String.join separator (List.map String.fromFloat nums)


joinPoint : List Float -> String
joinPoint point =
    joinNums "," point


joinPoints : List (List Float) -> String
joinPoints points =
    String.join " " (List.map joinPoint points)

flip3 : (a -> b -> c -> retval) -> c -> b -> a -> retval
flip3 function a b c =
    function c b a
