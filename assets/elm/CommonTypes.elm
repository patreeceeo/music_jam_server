module CommonTypes exposing (..)
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

