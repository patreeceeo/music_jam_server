module Errors exposing (instrumentDecoding)


instrumentDecoding : String -> String
instrumentDecoding details =
    "Uh oh there was an error! Looks like the programmers goofed up the JSON encoding/decoding. Details: " ++ details
