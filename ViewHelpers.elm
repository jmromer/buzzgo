module ViewHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


primaryButton : msg -> String -> Html msg
primaryButton msg buttonLabel =
    button [ class "primary", onClick msg ] [ text buttonLabel ]


alert : msg -> Maybe String -> Html msg
alert msg alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick msg ] [ text "x" ]
                , text message
                ]

        _ ->
            text ""
