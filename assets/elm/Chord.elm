module Chord exposing (Chord, ChordTree(..), Names(..), chords, decodeNames, decoder, forName, identify, ints, nameIntStrPairs, nameToInt, nameToStr, names, next, none, previous, rootTree, treeFor)

import Array exposing (Array)
import CommonTypes exposing (NoteVIndex)
import Dict
import Json.Decode as D
import List.Extra
import Maybe.Extra



{- Don't forget to update the lists below when adding new chords! -}


type Names
    = X_Chord
    | A_Chord
    | G_Chord
    | C_Chord


first : Names
first =
    X_Chord


last : Names
last =
    C_Chord


names : List Names
names =
    [ first, A_Chord, G_Chord, last ]


strs : List String
strs =
    [ "Other", "A", "G", "C" ]


chords : List Chord
chords =
    [ none
    , [ Nothing, Just 0, Just 2, Just 2, Just 2, Just 0 ]
    , [ Nothing, Just 3, Just 2, Just 0, Just 1, Just 0 ]
    , [ Just 3, Just 2, Just 0, Just 0, Just 0, Just 3 ]
    ]


namesArray : Array Names
namesArray =
    Array.fromList names


ints : List Int
ints =
    List.indexedMap (\i _ -> i) names


nameIntStrPairs : List ( Int, String )
nameIntStrPairs =
    List.Extra.zip ints strs


nameToInt : Names -> Int
nameToInt needle =
    names
        |> List.Extra.findIndex (\name -> name == needle)
        |> Maybe.withDefault 0


nameToStr : Names -> String
nameToStr needle =
    strs
        |> List.Extra.getAt (nameToInt needle)
        |> Maybe.withDefault "Other"


decoder : D.Decoder Names
decoder =
    D.oneOf
        [ D.int
            |> D.andThen
                (\int ->
                    case namesArray |> Array.get int of
                        Just name ->
                            D.succeed name

                        Nothing ->
                            D.fail ("Invalid chord comparable " ++ String.fromInt int)
                )
        , D.fail "Expected an int"
        ]


decodeNames : String -> Names
decodeNames input =
    input
        |> String.toInt
        |> Maybe.andThen
            (\int ->
                namesArray
                    |> Array.get int
            )
        |> Maybe.withDefault X_Chord



{- Musical chord. For each voice, this says whether that voice participates in the chord and with which note, referring to the visual representation.
   E.g. Am = [ (Just 0), (Just 1), (Just 2), (Just 2), (Just 0), Nothing ]
-}


type alias Chord =
    List (Maybe NoteVIndex)


none : Chord
none =
    List.map (\_ -> Just 0) (List.range 0 5)


dictIntToChord : Dict.Dict Int Chord
dictIntToChord =
    chords
        |> List.Extra.zip ints
        |> Dict.fromList


forName : Names -> Chord
forName name =
    dictIntToChord
        |> Dict.get (nameToInt name)
        |> Maybe.withDefault none


previous : Names -> Names
previous current =
    namesArray
        |> Array.get (nameToInt current - 1)
        |> Maybe.withDefault last


next : Names -> Names
next current =
    namesArray
        |> Array.get (nameToInt current + 1)
        |> Maybe.withDefault first


identify : Chord -> Result String Names
identify chord =
    case identifyStep (Identifying chord 0 rootTree) of
        IdentificationComplete name ->
            Ok name

        _ ->
            Err "Identification process ended prematurely"


type ChordTree
    = ChordTree (Dict.Dict NoteVIndex ChordTree)
    | ChordLeaf Names


getNote : Int -> Chord -> Maybe NoteVIndex
getNote voiceIndex chord =
    chord
        |> List.Extra.getAt voiceIndex
        |> Maybe.Extra.join


treeFor : Int -> List ( Int, Chord ) -> ChordTree
treeFor voiceIndex inChords =
    inChords
        {- Get note at voiceIndex paired with chord int -}
        |> (List.map <|
                \( int, chord ) ->
                    chord
                        |> getNote voiceIndex
                        |> Maybe.withDefault -1
                        |> Tuple.pair int
           )
        {- Group by note -}
        |> (List.Extra.groupWhile <|
                \( _, note1 ) ( _, note2 ) ->
                    note1 == note2
           )
        {- Create leaf if group size==1 else create a sub-tree -}
        |> (List.map <|
                \( ( int1, note1 ), rest ) ->
                    if List.length rest == 0 then
                        names
                            |> List.Extra.getAt int1
                            |> Maybe.withDefault X_Chord
                            |> ChordLeaf
                            |> Tuple.pair note1

                    else
                        rest
                            |> (++) [ ( int1, note1 ) ]
                            {- Map ints to chords -}
                            |> (List.map <|
                                    \( int, _ ) ->
                                        chords
                                            |> List.Extra.getAt int
                                            |> Maybe.withDefault none
                                            |> Tuple.pair int
                               )
                            |> treeFor (voiceIndex + 1)
                            |> Tuple.pair note1
           )
        |> Dict.fromList
        |> ChordTree


rootTree : ChordTree
rootTree =
    treeFor 0
        (chords
            |> List.indexedMap Tuple.pair
            |> List.tail
            |> Maybe.withDefault []
        )


type IdentificationProcess
    = Identifying Chord Int ChordTree
    | IdentificationComplete Names


identifyStep : IdentificationProcess -> IdentificationProcess
identifyStep process =
    case process of
        Identifying chord voiceIndex tree ->
            case tree of
                ChordTree properTree ->
                    let
                        subTree =
                            properTree
                                |> Dict.get (getNote voiceIndex chord |> Maybe.withDefault -1)
                                |> Maybe.withDefault (ChordLeaf X_Chord)
                    in
                    identifyStep (Identifying chord (voiceIndex + 1) subTree)

                ChordLeaf name ->
                    IdentificationComplete name

        _ ->
            process
