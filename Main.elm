module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Set exposing (fromList, member, size)
import Task exposing (perform)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { currentPlayer : TicTacToe
    , gameState : GameState
    , playCounter : Int
    , remainingOPossibilities : List Row
    , remainingXPossibilities : List Row
    , winningRow : Maybe Row
    , matrix : Dict CellID Cell
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { currentPlayer = X
      , gameState = Play
      , playCounter = 0
      , remainingOPossibilities = winList
      , remainingXPossibilities = winList
      , winningRow = Nothing
      , matrix =
            keys
                |> List.map
                    (\k ->
                        ( k
                        , { id = k
                          , entry = Empty
                          , isPartOfWin = False
                          }
                        )
                    )
                |> Dict.fromList
      }
    , Random.generate GameStart coinFlip
    )



-- TYPES


type Msg
    = Selection CellID
    | CheckWin
    | GameStart TicTacToe
    | Reset


type GameState
    = Play
    | Over GameEndState


type GameEndState
    = Win TicTacToe
    | Stalemate


type TicTacToe
    = X
    | O


type Entry
    = Empty
    | Selected TicTacToe


type alias Cell =
    { id : CellID
    , entry : Entry
    , isPartOfWin : Bool
    }


type alias CellID =
    Int


type alias Row =
    List CellID


coinFlip : Random.Generator TicTacToe
coinFlip =
    Random.map
        (\b ->
            if b then
                X
            else
                O
        )
        Random.bool


emptyCell : Cell
emptyCell =
    Cell -1 Empty False


winList : List Row
winList =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 7, 8, 9 ]
    , [ 1, 4, 7 ]
    , [ 2, 5, 8 ]
    , [ 3, 6, 9 ]
    , [ 1, 5, 9 ]
    , [ 3, 5, 7 ]
    ]


keys : List number
keys =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameStart player ->
            { model | currentPlayer = player } ! []

        Selection cellId ->
            let
                ( newRemainingXPossibilities, newRemainingOPossibilities, nextPlayer ) =
                    case model.currentPlayer of
                        X ->
                            ( model.remainingXPossibilities
                            , filterList model.remainingOPossibilities cellId
                            , O
                            )

                        O ->
                            ( filterList model.remainingXPossibilities cellId
                            , model.remainingOPossibilities
                            , X
                            )

                updatedCell : Cell
                updatedCell =
                    Cell cellId (Selected model.currentPlayer) False

                updatedMatrix =
                    model.matrix |> Dict.update cellId (\_ -> Just updatedCell)

                cmd =
                    if (model.playCounter + 1) > 3 then
                        Task.succeed CheckWin
                            |> Task.perform identity
                    else
                        Cmd.none
            in
            ( { model
                | currentPlayer = nextPlayer
                , remainingOPossibilities = newRemainingOPossibilities
                , remainingXPossibilities = newRemainingXPossibilities
                , playCounter = model.playCounter + 1
                , matrix = updatedMatrix
              }
            , cmd
            )

        CheckWin ->
            let
                checkList =
                    case model.currentPlayer of
                        X ->
                            model.remainingOPossibilities

                        O ->
                            model.remainingXPossibilities

                winRow =
                    checkWin model.matrix checkList

                isWin =
                    if List.length winRow == 1 then
                        winRow
                            |> List.head
                    else
                        Nothing

                updatedMatrix =
                    case isWin of
                        Just winRow ->
                            updateWins model.matrix winRow

                        Nothing ->
                            model.matrix

                gameState =
                    if (List.isEmpty model.remainingOPossibilities || List.isEmpty model.remainingXPossibilities) && isWin == Nothing then
                        Over Stalemate
                    else if isWin /= Nothing then
                        otherGuy model.currentPlayer
                            |> Win
                            |> Over
                    else
                        Play
            in
            ( { model
                | winningRow = isWin
                , gameState = gameState
                , matrix = updatedMatrix
              }
            , Cmd.none
            )

        Reset ->
            init


otherGuy : TicTacToe -> TicTacToe
otherGuy ticTacToe =
    case ticTacToe of
        X ->
            O

        O ->
            X



{-
   updateWins - the idea is to create a dict of updated cells and then
   union it to the existing dict
   first step map over the ids and extract the cells,
   second step - in the same iteration create updated cells ultimatly
   generate a new dict. then use union to join the rest of the existing dict
-}


updateWins : Dict CellID Cell -> List CellID -> Dict CellID Cell
updateWins matrix row =
    let
        winningCells =
            row
                |> List.map
                    (\cellId ->
                        let
                            cell =
                                Dict.get cellId matrix
                                    |> Maybe.withDefault emptyCell

                            updatedCell =
                                { cell | isPartOfWin = True }
                        in
                        ( cellId, updatedCell )
                    )
                |> Dict.fromList
    in
    Dict.union winningCells matrix


checkWin : Dict CellID Cell -> List Row -> List Row
checkWin matrix list =
    let
        newList =
            list
                |> List.filter (\row -> checkLine matrix row)
    in
    newList



{-
   checkLine - a filter function for checkWin, checks to see if a row of cells are
   the same, it inserts the entries into a set,
   if they're all the same the size will be == 1
-}


checkLine : Dict CellID Cell -> Row -> Bool
checkLine matrix row =
    let
        winningSet =
            row
                |> List.map
                    (\cellId ->
                        let
                            cell =
                                Dict.get cellId matrix
                                    |> Maybe.withDefault emptyCell
                        in
                        toString cell.entry
                    )
                |> Set.fromList
    in
    Set.size winningSet == 1 && (Set.member "Empty" winningSet |> not)


filterList : List Row -> CellID -> List Row
filterList list cell =
    list
        |> List.filter
            (\row ->
                hasCell row cell
            )


hasCell : List a -> a -> Bool
hasCell row cell =
    let
        newRow =
            row
                |> not
                << List.member cell
    in
    newRow



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ mainContainerStyle ]
        [ displayCurrentPlayer model.currentPlayer model.gameState
        , displayTicTacToe model
        ]


displayTicTacToe : Model -> Html Msg
displayTicTacToe { matrix } =
    div []
        [ div [ containerStyle ]
            [ div [] [ getMatrixCell matrix 1 ]
            , div [] [ getMatrixCell matrix 2 ]
            , div [] [ getMatrixCell matrix 3 ]
            ]
        , div [ containerStyle ]
            [ div [] [ getMatrixCell matrix 4 ]
            , div [] [ getMatrixCell matrix 5 ]
            , div [] [ getMatrixCell matrix 6 ]
            ]
        , div [ containerStyle ]
            [ div [] [ getMatrixCell matrix 7 ]
            , div [] [ getMatrixCell matrix 8 ]
            , div [] [ getMatrixCell matrix 9 ]
            ]
        ]


displayCurrentPlayer : TicTacToe -> GameState -> Html Msg
displayCurrentPlayer ticTacToe gameState =
    let
        nextGameLink =
            case gameState of
                Play ->
                    text ""

                Over becauseOf ->
                    case becauseOf of
                        Win winner ->
                            a [ onClick Reset ]
                                [ "Congratulations " ++ toString winner ++ " won! " |> text
                                , text " New game"
                                ]

                        Stalemate ->
                            a [ onClick Reset ]
                                [ "Another stalemate " |> text
                                , text "New game"
                                ]
    in
    div []
        [ toString ticTacToe ++ " " |> text
        , nextGameLink
        ]


displayEmptyCell : CellID -> Html Msg
displayEmptyCell cellNum =
    div
        [ cellStyle
        , onClick (Selection cellNum)
        ]
        []


displayFilledCell : TicTacToe -> Bool -> Html msg
displayFilledCell ticTacToe isWin =
    let
        displayStyle =
            if isWin == True then
                winStyle
            else
                style [ ( "", "" ) ]
    in
    div [ cellStyle, displayStyle ] [ text (toString ticTacToe) ]


getMatrixCell : Dict CellID Cell -> CellID -> Html Msg
getMatrixCell matrix cellNum =
    case Dict.get cellNum matrix of
        Just cell ->
            case cell.entry of
                Empty ->
                    displayEmptyCell cellNum

                Selected xo ->
                    displayFilledCell xo cell.isPartOfWin

        Nothing ->
            text ""



-- Styles


cellStyle : Html.Attribute msg
cellStyle =
    style
        [ ( "border", "solid thin lightblue" )
        , ( "width", "100px" )
        , ( "height", "100px" )
        , ( "vertical-align", "middle" )
        , ( "text-align", "center" )
        , ( "font-size", "85px" )
        ]


winStyle : Html.Attribute msg
winStyle =
    style [ ( "color", "green" ) ]


lineStyle : Html.Attribute msg
lineStyle =
    style
        [ ( "border", "solid thin lightblue" )
        , ( "width", "100px" )
        , ( "height", "100px" )
        ]


containerStyle : Html.Attribute msg
containerStyle =
    style
        [ ( "display", "flex" )
        , ( "flex", "1" )
        ]


mainContainerStyle : Html.Attribute msg
mainContainerStyle =
    style
        [ ( "display", "flex" )
        , ( "flex-direction", "column" )
        , ( "align-items", "center" )
        ]
