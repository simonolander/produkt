module Main exposing (..)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import List exposing (all, concat, foldl, indexedMap, isEmpty, length, maximum, member, singleton, sort)
import List.Extra exposing (remove, transpose, updateAt, zip)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (values)
import Random exposing (Generator, int, list)
import Random.Extra exposing (choice, oneIn)
import Random.List
import String exposing (fromInt)
import Tuple exposing (first)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


type ProductState
    = Incorrect
    | Correct
    | Incomplete
    | TooHigh


type alias Model =
    Board


type alias Board =
    List (List Cell)


type alias Cell =
    { state : Maybe Direction
    , value : Int
    , facit : Direction
    , hint : Bool
    }


type alias Target =
    { achieved : Bool
    , value : Int
    }


type alias Product =
    { state : ProductState
    , value : Int
    }


type alias Score =
    { targets : List Target
    , rowProducts : List Product
    , columnProducts : List Product
    }


type Direction
    = Row
    | Column


fromDirection : Direction -> String
fromDirection cellState =
    case cellState of
        Row ->
            "row"

        Column ->
            "column"


boardWidth : Int
boardWidth =
    4


boardHeight : Int
boardHeight =
    4


generateBoardCommand : Cmd Msg
generateBoardCommand =
    Random.generate GeneratedBoard (boardGenerator boardWidth boardHeight)


init : () -> ( Model, Cmd Msg )
init =
    always ( [], generateBoardCommand )


boardGenerator : Int -> Int -> Generator Board
boardGenerator width height =
    let
        valueGenerator : Generator Int
        valueGenerator =
            int 2 9

        cellStateGenerator : Generator Direction
        cellStateGenerator =
            choice Row Column

        hintGenerator : Generator Bool
        hintGenerator =
            oneIn 10

        cellGenerator : Generator Cell
        cellGenerator =
            Random.map3 (Cell Nothing) valueGenerator cellStateGenerator hintGenerator
    in
    list height (list width cellGenerator)


getScore : Board -> Score
getScore board =
    let
        boardT : Board
        boardT =
            transpose board

        multiplyCellValues : (Cell -> Bool) -> List Cell -> Int
        multiplyCellValues predicate cells =
            List.filter predicate cells
                |> List.map .value
                |> List.product

        hasState : Direction -> Cell -> Bool
        hasState state cell =
            cell.state == Just state

        hasFacit : Direction -> Cell -> Bool
        hasFacit state cell =
            cell.facit == state

        hasDirection : Direction -> Cell -> Bool
        hasDirection direction cell =
            (if cell.hint then
                hasFacit

             else
                hasState
            )
                direction
                cell

        rowProductValues : List Int
        rowProductValues =
            List.map (multiplyCellValues (hasDirection Row)) board

        columnProductValues : List Int
        columnProductValues =
            List.map (multiplyCellValues (hasDirection Column)) boardT

        currentProducts : List Int
        currentProducts =
            rowProductValues ++ columnProductValues

        targetValues : List Int
        targetValues =
            sort <|
                List.concat
                    [ List.map (multiplyCellValues (hasFacit Row)) board
                    , List.map (multiplyCellValues (hasFacit Column)) boardT
                    ]

        maximumTargetValue : Int
        maximumTargetValue =
            withDefault 0 (maximum targetValues)

        computeTargets : List Int -> List Int -> List Target
        computeTargets remainingTargetValues remainingCurrentValues =
            case remainingTargetValues of
                value :: vs ->
                    let
                        achieved : Bool
                        achieved =
                            member value remainingCurrentValues

                        target : Target
                        target =
                            { achieved = achieved, value = value }
                    in
                    target :: computeTargets vs (remove value remainingCurrentValues)

                [] ->
                    []

        computeProducts : List ( List Cell, Int ) -> List Int -> List Product
        computeProducts remainingCurrentValues remainingTargetValues =
            case remainingCurrentValues of
                ( cells, value ) :: vs ->
                    let
                        state : ProductState
                        state =
                            if member value remainingTargetValues then
                                Correct

                            else if value > maximumTargetValue then
                                TooHigh

                            else if all (\cell -> cell.hint || cell.state /= Nothing) cells then
                                Incorrect

                            else
                                Incomplete

                        target : Product
                        target =
                            { state = state
                            , value = value
                            }
                    in
                    target :: computeProducts vs (remove value remainingTargetValues)

                [] ->
                    []

        targets =
            computeTargets targetValues currentProducts

        rowsProducts =
            computeProducts (zip board rowProductValues) targetValues

        remainingTargetValuesAfterRows =
            foldl remove targetValues rowProductValues

        columnProducts =
            computeProducts (zip boardT columnProductValues) remainingTargetValuesAfterRows
    in
    { targets = targets
    , rowProducts = rowsProducts
    , columnProducts = columnProducts
    }



-- UPDATE


type Msg
    = GeneratedBoard Board
    | GeneratedHint Int Int
    | ClickedCell Int Int
    | ClickedClearRow Int
    | ClickedClearColumn Int
    | ClickedClearBoard
    | ClickedHint
    | ClickedNewGame


updateCell : (Cell -> Cell) -> Int -> Int -> Board -> Board
updateCell f rowIndex columnIndex board =
    updateAt rowIndex (updateAt columnIndex f) board


toggle : Cell -> Cell
toggle cell =
    { cell
        | state =
            case cell.state of
                Just Row ->
                    Just Column

                Just Column ->
                    Nothing

                Nothing ->
                    Just Row
    }


clearCell : Cell -> Cell
clearCell cell =
    { cell | state = Nothing }


hintCell : Cell -> Cell
hintCell cell =
    { cell | hint = True }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg board =
    case msg of
        GeneratedBoard newBoard ->
            ( newBoard, Cmd.none )

        ClickedCell rowIndex colIndex ->
            ( updateCell toggle rowIndex colIndex board, Cmd.none )

        ClickedClearRow rowIndex ->
            ( updateAt rowIndex (List.map clearCell) board, Cmd.none )

        ClickedClearColumn columnIndex ->
            ( transpose board |> updateAt columnIndex (List.map clearCell) |> transpose, Cmd.none )

        ClickedClearBoard ->
            ( List.map (List.map clearCell) board, Cmd.none )

        GeneratedHint rowIndex columnIndex ->
            ( updateCell hintCell rowIndex columnIndex board, Cmd.none )

        ClickedHint ->
            let
                takeIfNotHint : Int -> Int -> Cell -> Maybe ( Int, Int )
                takeIfNotHint rowIndex columnIndex cell =
                    if cell.hint then
                        Nothing

                    else
                        Just ( rowIndex, columnIndex )

                takeIfNotHints : Int -> List Cell -> List ( Int, Int )
                takeIfNotHints rowIndex row =
                    values <| indexedMap (takeIfNotHint rowIndex) row

                hintPositionGenerator : Generator ( Int, Int )
                hintPositionGenerator =
                    indexedMap takeIfNotHints board
                        |> concat
                        |> Random.List.choose
                        |> Random.map first
                        |> Random.map (withDefault ( 0, 0 ))

                cmd : Cmd Msg
                cmd =
                    Random.generate (uncurry GeneratedHint) hintPositionGenerator
            in
            ( board, cmd )

        ClickedNewGame ->
            ( board, generateBoardCommand )



-- VIEW


view : Model -> Html Msg
view model =
    if isEmpty model then
        viewLoading

    else
        viewLoaded model


viewLoading : Html msg
viewLoading =
    text "Loading..."


viewLoaded : Board -> Html Msg
viewLoaded board =
    let
        score : Score
        score =
            getScore board

        completed : Bool
        completed =
            List.all .achieved score.targets
    in
    div [ class "main" ]
        [ viewBoard board score
        , div
            [ class "sidebar" ]
            [ viewTargets score.targets
            , viewControls completed
            ]
        ]


viewBoard : Board -> Score -> Html Msg
viewBoard board score =
    let
        rowViews : List (Html Msg)
        rowViews =
            List.indexedMap viewRow (zip board score.rowProducts)

        clearButton =
            button
                [ onClick ClickedClearBoard
                ]
                [ text "c" ]

        productsView =
            div
                [ class "column-products" ]
                (indexedMap viewColumnProduct score.columnProducts ++ [ clearButton ])

        contents : List (Html Msg)
        contents =
            rowViews ++ [ productsView ]
    in
    div [ class "board" ] contents


viewRow : Int -> ( List Cell, Product ) -> Html Msg
viewRow rowIndex ( row, product ) =
    let
        cellViews : List (Html Msg)
        cellViews =
            List.indexedMap (viewCell rowIndex) row

        productView : Html Msg
        productView =
            viewRowProduct rowIndex product

        contents : List (Html Msg)
        contents =
            cellViews ++ [ productView ]
    in
    div [ class "row" ] contents


viewCell : Int -> Int -> Cell -> Html Msg
viewCell rowIndex columnIndex { state, facit, hint, value } =
    let
        stateClass =
            if hint then
                fromDirection facit

            else
                Maybe.map fromDirection state |> withDefault "blank"
    in
    button
        [ class "cell"
        , class stateClass
        , disabled hint
        , onClick (ClickedCell rowIndex columnIndex)
        ]
        [ text <| fromInt value ]


viewRowProduct : Int -> Product -> Html Msg
viewRowProduct rowIndex product =
    button
        [ class "row-product"
        , onClick (ClickedClearRow rowIndex)
        ]
        [ text <| fromInt product.value ]


viewColumnProduct : Int -> Product -> Html Msg
viewColumnProduct rowIndex product =
    button
        [ class "column-product"
        , onClick (ClickedClearColumn rowIndex)
        ]
        [ text <| fromInt product.value ]


viewTargets : List Target -> Html msg
viewTargets targets =
    div [ class "targets" ] <| List.map viewTarget targets


viewTarget : Target -> Html msg
viewTarget target =
    span [ class "target" ]
        [ text (fromInt target.value) ]


viewControls completed =
    let
        contents =
            singleton <|
                if completed then
                    button
                        [ class "control"
                        , onClick ClickedNewGame
                        ]
                        [ text "New game" ]

                else
                    button
                        [ class "control"
                        , onClick ClickedHint
                        ]
                        [ text "Hint" ]
    in
    div
        [ class "controls" ]
        contents
