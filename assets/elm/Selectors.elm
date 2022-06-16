module Selectors exposing (Selectors)

import KbdEvent


type alias Selectors =
    { milisSinceKeyDown : KbdEvent.Key -> Int
    , timeInMillis : () -> Int
    , screenWidth : () -> Int
    }
