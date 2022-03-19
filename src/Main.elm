module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Random exposing (Generator, int, list)
import Random.Extra exposing (choice, oneIn)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


type Model
    = Loading
    | Loaded Board


type alias Board =
    List (List Cell)


type alias Cell =
    { state : Maybe CellState
    , value : Int
    , facit : CellState
    , hint : Bool
    }


type CellState
    = Right
    | Down


init : () -> ( Model, Cmd Msg )
init =
    always ( Loading, Random.generate GeneratedBoard (boardGenerator 5 5) )


boardGenerator : Int -> Int -> Generator Board
boardGenerator width height =
    let
        valueGenerator : Generator Int
        valueGenerator =
            int 2 9

        cellStateGenerator : Generator CellState
        cellStateGenerator =
            choice Right Down

        hintGenerator : Generator Bool
        hintGenerator =
            oneIn 10

        cellGenerator : Generator Cell
        cellGenerator =
            Random.map3 (Cell Nothing) valueGenerator cellStateGenerator hintGenerator
    in
    list height (list width cellGenerator)



-- UPDATE


type Msg
    = GeneratedBoard Board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedBoard board ->
            ( Loaded board, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            viewLoading

        Loaded board ->
            viewLoaded board


viewLoading : Html msg
viewLoading =
    text "Loading..."


viewLoaded : Board -> Html msg
viewLoaded board =
    div []
        [ viewBoard board
        ]


viewBoard board =
    div [ class "board" ] <| List.map viewRow board


viewRow row =
    text "Row"
