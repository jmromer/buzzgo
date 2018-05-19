module ViewHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


primaryButton : msg -> String -> Html msg
primaryButton msg buttonLabel =
    button [ class "primary", onClick msg ] [ text buttonLabel ]
