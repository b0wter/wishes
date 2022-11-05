module WelcomePage exposing (..)

import Html exposing (Html, div, h1, text)
import Main exposing (Msg)

view : Html Msg
view =
    div []
    [ h1 [] [ text "Welcome =)" ]
    ]