module UtilsTests exposing (..)
import Utils exposing (joinPoints)

import Expect
import Test exposing (..)

suite : Test
suite =
  describe "Main module"
    [ test "joinPoints" <|
      \_ ->
        Expect.equal "0,0 0,1 1,1 1,0" (joinPoints [[0,0],[0,1],[1,1],[1,0]])
    ]
