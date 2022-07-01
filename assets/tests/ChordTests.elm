module ChordTests exposing (suite)

import Chord exposing (ChordTree(..), Names(..), identify, rootTree)
import Dict
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Chord"
        [ describe "names" testIdentify
        , describe "tree" testTree
        ]


testIdentify : List Test
testIdentify =
    Chord.names
        |> (List.map <|
                \name ->
                    test (Chord.nameToStr name) <|
                        \_ ->
                            Expect.equal (Ok name) (identify (Chord.forName name))
           )


testTree : List Test
testTree =
    [ test "typical case" <|
        \_ ->
            let
                actual =
                    rootTree

                expected =
                    ChordTree
                        (Dict.fromList
                            [ ( -1, ChordTree (Dict.fromList [ ( 0, ChordLeaf A_Chord ), ( 3, ChordLeaf G_Chord ) ]) )
                            , ( 3, ChordLeaf C_Chord )
                            ]
                        )
            in
            Expect.equal actual expected
    ]
