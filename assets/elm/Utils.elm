module Utils exposing (joinPoints, joinNums)


joinNums : String -> List Float -> String
joinNums separator nums =
    String.join separator (List.map String.fromFloat nums)


joinPoint : List Float -> String
joinPoint point =
    joinNums "," point


joinPoints : List (List Float) -> String
joinPoints points =
    String.join " " (List.map joinPoint points)
