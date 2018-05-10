module Buzzgo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Random


-- Update


type Msg
    = NewGame
    | Mark Int
    | ShareScore
    | Sort
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randnum ->
            ( { model | gameNumber = randnum }
            , Cmd.none
            )

        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }
            , getEntries
            )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e
            in
                ( { model | entries = List.map markEntry model.entries }
                , Cmd.none
                )

        Sort ->
            ( { model | entries = (List.sortBy .points model.entries) }
            , Cmd.none
            )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = randomEntries }, Cmd.none )

        NewEntries (Err error) ->
            let
                errorMessage =
                    case error of
                        Http.NetworkError ->
                            "Is the server running?"

                        Http.BadStatus response ->
                            (toString response.status)

                        Http.BadPayload message _ ->
                            "Decoding Failed: " ++ message

                        _ ->
                            (toString error)
            in
                ( { model | alertMessage = Just errorMessage }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Decoders


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)



-- Commands


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


entriesUrl : String
entriesUrl =
    "http://localhost:3000/random-entries"


getEntries : Cmd Msg
getEntries =
    (Decode.list entryDecoder)
        |> Http.get entriesUrl
        |> Http.send NewEntries



-- Model


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    , alertMessage : Maybe String
    }


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }



-- initialModel :


initialModel : Model
initialModel =
    { name = "Mike"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    }


allEntriesMarked : List Entry -> Bool
allEntriesMarked entries =
    List.all .marked entries


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.foldl (+) 0



-- View


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " -- Game #" ++ (toString gameNumber)


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
        h2 [ id "info", class "classy" ] [ playerInfoText ]


viewHeader : String -> Html Msg
viewHeader title =
    div []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "https://elm-lang-org" ]
            [ text "Powered by Elm" ]
        ]


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li
        [ classList [ ( "marked", entry.marked ) ]
        , onClick (Mark entry.id)
        ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    let
        listOfEntries =
            List.map viewEntryItem entries
    in
        ul [] listOfEntries


viewScore : Int -> Html Msg
viewScore score =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString score) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "Buzzword Bingo"
        , viewPlayer model.name model.gameNumber
        , viewAlertMessage model.alertMessage
        , viewEntryList model.entries
        , viewScore (sumMarkedPoints model.entries)
        , div
            [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            , button [ onClick Sort ] [ text "Sort" ]
            ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick CloseAlert ] [ text "x" ]
                , text message
                ]

        _ ->
            text ""


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
