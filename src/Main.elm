module Main exposing (..)

import Basics.Extra exposing (uncurry)
import Browser exposing (Document)
import BuildInfo exposing (buildTime, version)
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, disabled, href, title)
import Html.Events exposing (onClick)
import List exposing (all, concat, foldl, indexedMap, isEmpty, maximum, member, singleton, sort)
import List.Extra exposing (remove, transpose, updateAt, zip)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (values)
import Random exposing (Generator, constant, generate, int, list, uniform)
import Random.Extra exposing (choice)
import String exposing (fromInt)



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


classFromProductState : ProductState -> String.String
classFromProductState productState =
    case productState of
        Incorrect ->
            "incorrect"

        Correct ->
            "correct"

        Incomplete ->
            "incomplete"

        TooHigh ->
            "too-high"


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


classFromDirection : Direction -> String
classFromDirection direction =
    case direction of
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
            constant False

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

                candidatePositions : List ( Int, Int )
                candidatePositions =
                    concat <| indexedMap takeIfNotHints board

                maybePositionGenerator : Maybe (Generator ( Int, Int ))
                maybePositionGenerator =
                    case candidatePositions of
                        h :: t ->
                            Just (uniform h t)

                        [] ->
                            Nothing

                cmd : Cmd Msg
                cmd =
                    Maybe.map (generate (uncurry GeneratedHint)) maybePositionGenerator
                        |> withDefault Cmd.none
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
        [ a
            [ class "build-info"
            , href "https://github.com/simonolander/produkt"
            ]
            [ div
                [ class "build-time"
                , title "Build time"
                ]
                [ text buildTime ]
            , div
                [ class "version"
                , title "Version"
                ]
                [ text version ]
            ]
        , div
            [ class "center" ]
            [ viewBoard board score
            , div
                [ class "sidebar" ]
                [ viewTargets score.targets
                , viewControls completed
                ]
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
                , class "clear-all"
                ]
                [ text "🚮" ]

        productsView =
            div
                [ class "column-products" ]
                (indexedMap (viewProduct Column) score.columnProducts ++ [ clearButton ])

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
            viewProduct Row rowIndex product

        contents : List (Html Msg)
        contents =
            cellViews ++ [ productView ]
    in
    div [ class "board-row" ] contents


viewCell : Int -> Int -> Cell -> Html Msg
viewCell rowIndex columnIndex { state, facit, hint, value } =
    let
        stateClass =
            if hint then
                classFromDirection facit

            else
                Maybe.map classFromDirection state |> withDefault "blank"
    in
    button
        [ class "cell"
        , class stateClass
        , disabled hint
        , onClick (ClickedCell rowIndex columnIndex)
        ]
        [ text <| fromInt value ]


viewProduct : Direction -> Int -> Product -> Html Msg
viewProduct direction index product =
    let
        directionClass =
            classFromDirection direction

        productStateClass =
            classFromProductState product.state

        clickedMessage =
            case direction of
                Row ->
                    ClickedClearRow

                Column ->
                    ClickedClearColumn
    in
    button
        [ class directionClass
        , class "product"
        , class productStateClass
        , onClick (clickedMessage index)
        , title "Click to clear line"
        ]
        [ text <| fromInt product.value ]


viewTargets : List Target -> Html msg
viewTargets targets =
    div [ class "targets" ] <| List.map viewTarget targets


viewTarget : Target -> Html msg
viewTarget target =
    let
        achievedClass =
            if target.achieved then
                "achieved"

            else
                ""
    in
    div
        [ class "target"
        , class achievedClass
        ]
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
                        [ text "✅" ]

                else
                    button
                        [ class "control"
                        , onClick ClickedHint
                        ]
                        [ text "💡" ]
    in
    div
        [ class "controls" ]
        contents
