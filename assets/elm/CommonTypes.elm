module CommonTypes exposing (..)
import KbdEvent

{- Note representation which corresponds to the visual presentation of the instrument, e.g. which number fret on a guitar's fretboard. -}


type alias NoteVIndex =
    Int



{- The pitch (frequency) of a note (TODO what unit?) -}


type alias Pitch =
    Float



{- Decibles? -}


type alias Volume =
    Float



{- Time measured in whole numbers, normally milliseconds -}


type alias QTime =
    Int


type Inputs
    = SelectChordInput
    | NoInput

type Routes
    = FaqRoute
    | MainRoute
    | SelectChordRoute

type alias Selectors =
    { milisSinceKeyDown : KbdEvent.Key -> Int
    , timeInMillis : () -> Int
    , screenWidth : () -> Int
    , currentRoute : () -> Maybe Routes
    }
