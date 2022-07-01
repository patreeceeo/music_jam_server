module DebugHelpers exposing (..)

import Chord


debugChordName : Chord.Names -> Chord.Names
debugChordName chord =
    Debug.log "chord name" chord
