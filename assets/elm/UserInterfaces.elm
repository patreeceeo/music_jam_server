module UI exposing (instrument)
import Html

instrument : Model -> Html.Html Msg
instrument model = 
                [ Svg.svg
                    [ class "instrument"
                    , preserveAspectRatio "xMidYMid meet"
                    , viewBox viewBox_
                    ]
                    ([ svgDefs
                     , outerPoly
                     , frets
                     ]
                        ++ viewInlays
                        ++ viewStrings model
                        ++ viewDebugging instrument
                    )
                ]
