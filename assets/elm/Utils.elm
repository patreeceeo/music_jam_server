module Utils exposing (PathSegment, TestableNavKey(..), debugUnless, flip3, joinAnimationValues, joinNums, joinPathSegments, joinPoints, loopInt, mapAccumr, tagReturnWith, tagReturnWithP2, untagP1, untagP2)

import Browser.Navigation



-- Like List.Extra.mapAccumr but simpler


type TestableNavKey
    = ActualNavKey Browser.Navigation.Key
    | TestNavKey


mapAccumr : (a -> b -> a) -> a -> List b -> a
mapAccumr f acc0 list =
    List.foldr
        (\x acc1 ->
            f acc1 x
        )
        acc0
        list


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


tagReturnWith : (untagged -> tagged) -> (input -> untagged) -> (input -> tagged)
tagReturnWith tag f param =
    tag (f param)


tagReturnWithP2 : (untagged -> tagged) -> (a -> b -> untagged) -> (a -> b -> tagged)
tagReturnWithP2 tag f a b =
    tag (f a b)


untagP1 : (tagged -> p1) -> (p1 -> p2 -> return) -> (tagged -> p2 -> return)
untagP1 untag f tagged p2 =
    f (untag tagged) p2


untagP2 : (tagged -> p2) -> (p1 -> p2 -> return) -> (p1 -> tagged -> return)
untagP2 untag f p1 tagged =
    f p1 (untag tagged)


debugUnless : String -> msg -> (msg -> Bool) -> msg
debugUnless str msg f =
    if f msg then
        msg

    else
        Debug.log str msg
