module Buzzgo exposing (..)

import Entry
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode exposing (..)
import Random
import ViewHelpers exposing (primaryButton, alert)


-- Update


type Msg
    = NewGame
    | CloseAlert
    | Mark Int
    | NewEntries (Result Http.Error (List Entry.Entry))
    | NewRandom Int
    | NewScore (Result Http.Error Score)
    | SetNameInput String
    | ShareScore
    | Sort
    | SaveName
    | CancelName
    | ChangeGameState GameState


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none )

        SaveName ->
            if (String.isEmpty model.nameInput) then
                ( { model | nameInput = "" }, Cmd.none )
            else
                ( { model
                    | name = model.nameInput
                    , nameInput = ""
                    , gameState = Playing
                  }
                , Cmd.none
                )

        CancelName ->
            ( { model
                | name = "Anonymous"
                , nameInput = ""
                , gameState = Playing
              }
            , Cmd.none
            )

        SetNameInput value ->
            ( { model | name = value, nameInput = value }, Cmd.none )

        NewRandom randnum ->
            ( { model | gameNumber = randnum }, Cmd.none )

        ShareScore ->
            ( model, postScore model )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ (toString score.score)
                        ++ " was succesfully shared!"
            in
                ( { model | alertMessage = Just message }, Cmd.none )

        NewScore (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, Entry.getEntries NewEntries entriesUrl )

        Mark id ->
            ( { model | entries = (Entry.markEntryWithId model.entries id) }, Cmd.none )

        Sort ->
            ( { model | entries = (List.sortBy .points model.entries) }, Cmd.none )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = randomEntries }, Cmd.none )

        NewEntries (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.NetworkError ->
            "Is the server running?"

        Http.BadStatus response ->
            (toString response.status)

        Http.BadPayload message _ ->
            "Decoding Failed: " ++ message

        _ ->
            (toString error)



-- _ ->
--     ( model, Cmd.none )
-- Decoders / Encoders


encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "score", Encode.int (Entry.sumMarkedPoints model.entries) )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)



-- Commands


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


entriesUrl : String
entriesUrl =
    "http://localhost:3000/random-entries"


postScore : Model -> Cmd Msg
postScore model =
    let
        url =
            "http://localhost:3000/scores"

        body =
            encodeScore model
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
        Http.send NewScore request



-- Model


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry.Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }



-- initialModel :


initialModel : Model
initialModel =
    { name = "Anonymous"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = EnteringName
    }



-- View


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ] [ text name ]
        , text (" -- Game #" ++ (toString gameNumber))
        ]


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
        , alert CloseAlert model.alertMessage
        , viewNameInput model
        , Entry.viewEntryList Mark model.entries
        , viewScore (Entry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game"
            , primaryButton Sort "Sort"
            , primaryButton ShareScore "Share Score"
            ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        Playing ->
            text ""

        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , value model.nameInput
                    , onInput SetNameInput
                    ]
                    []
                , primaryButton SaveName "Save"
                , primaryButton CancelName "Cancel"
                ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Entry.getEntries NewEntries entriesUrl )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
