module Utils exposing (PathSegment, flip3, joinAnimationValues, joinNums, joinPathSegments, joinPoints, loopInt)


joinNums : String -> List Float -> String
joinNums separator nums =
    String.join separator (List.map String.fromFloat nums)


joinPoint : List Float -> String
joinPoint point =
    joinNums "," point


joinPoints : List (List Float) -> String
joinPoints points =
    String.join " " (List.map joinPoint points)


type alias PathSegment =
    { command : String
    , points : List (List Float)
    }


joinAnimationValues : List (List PathSegment) -> String
joinAnimationValues values =
    String.join ";" (List.map (\segments -> joinPathSegments segments) values)


joinPathSegments : List PathSegment -> String
joinPathSegments segments =
    String.join " " (List.map (\seg -> seg.command ++ joinPoints seg.points) segments)


flip3 : (a -> b -> c -> retval) -> c -> b -> a -> retval
flip3 function a b c =
    function c b a


loopInt : Int -> Int -> Int -> Int
loopInt min max curr =
    modBy (max - min) (curr - min) + min
