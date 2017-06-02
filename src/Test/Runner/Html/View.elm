module Test.Runner.Html.View
    exposing
        ( Model
        , uninitialized
        , initialized
        )

import Dict exposing (Dict)
import Set exposing (Set)
import String
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Expect exposing (Expectation)
import Test.Runner


type alias Model =
    { available : Dict Int (() -> ( List String, List Expectation ))
    , running : Set Int
    , queue : List Int
    , completed : List ( List String, List Expectation )
    , startTime : Time
    , finishTime : Maybe Time
    }


uninitialized : Html a
uninitialized =
    text ""


initialized : Model -> Html a
initialized model =
    let
        summary =
            case model.finishTime of
                Just finishTime ->
                    let
                        ( headlineColor, headlineText ) =
                            if List.isEmpty failures then
                                ( "darkgreen", "Test Run Passed" )
                            else
                                ( "hsla(3, 100%, 40%, 1.0)", "Test Run Failed" )

                        thStyle =
                            [ ( "text-align", "left" ), ( "padding-right", "10px" ) ]

                        duration =
                            formatDuration (finishTime - model.startTime)
                    in
                        div []
                            [ h2 [ style [ ( "color", headlineColor ) ] ] [ text headlineText ]
                            , table []
                                [ tbody []
                                    [ tr []
                                        [ th [ style thStyle ]
                                            [ text "Duration" ]
                                        , td []
                                            [ text duration ]
                                        ]
                                    , tr []
                                        [ th [ style thStyle ]
                                            [ text "Passed" ]
                                        , td []
                                            [ text (toString (completedCount - List.length failures)) ]
                                        ]
                                    , tr []
                                        [ th [ style thStyle ]
                                            [ text "Failed" ]
                                        , td []
                                            [ text (toString (List.length failures)) ]
                                        ]
                                    ]
                                ]
                            ]

                Nothing ->
                    div []
                        [ h2 [] [ text "Running Tests..." ]
                        , div [] [ text (toString completedCount ++ " completed") ]
                        , div [] [ text (toString remainingCount ++ " remaining") ]
                        ]

        completedCount =
            List.length model.completed

        remainingCount =
            List.length (Dict.keys model.available)

        failures : List ( List String, List Expectation )
        failures =
            List.filter (Tuple.second >> List.any ((/=) Expect.pass)) model.completed
    in
        div [ style [ ( "width", "960px" ), ( "margin", "auto 40px" ), ( "font-family", "verdana, sans-serif" ) ] ]
            [ summary
            , ol [ class "results", resultsStyle ] (List.map viewFailures failures)
            ]


viewLabels : List String -> List (Html a)
viewLabels =
    Test.Runner.formatLabels (withColorChar '↓' "darkgray")
        (withColorChar '✗' "hsla(3, 100%, 40%, 1.0)")


givenAttributes : List (Html.Attribute a)
givenAttributes =
    [ width 80
    , style
        [ ( "margin-bottom", "24px" )
        , ( "color", "darkgray" )
        , ( "font-size", "inherit" )
        , ( "font-family", "inherit" )
        ]
    ]


messageAttributes : List (Html.Attribute a)
messageAttributes =
    [ width 80
    , style
        [ ( "margin-left", "32px" )
        , ( "margin-bottom", "40px" )
        , ( "font-size", "inherit" )
        , ( "font-family", "inherit" )
        ]
    ]


withColorChar : Char -> String -> String -> Html a
withColorChar char textColor str =
    div [ style [ ( "color", textColor ) ] ]
        [ text (String.cons char (String.cons ' ' str)) ]


resultsStyle : Html.Attribute a
resultsStyle =
    style [ ( "font-size", "14px" ), ( "line-height", "1.3" ), ( "font-family", "Menlo, Consolas, \"Fira Mono\", \"DejaVu Sans Mono\", \"Liberation Monospace\", \"Liberation Mono\", Monaco, \"Lucida Console\", \"Courier New\", monospace" ) ]


viewFailures : ( List String, List Expectation ) -> Html a
viewFailures ( labels, failures ) =
    li [ style [ ( "margin", "40px 0" ) ] ]
        (viewLabels labels ++ (List.filterMap viewFailure failures))


viewFailure : Expectation -> Maybe (Html a)
viewFailure expectation =
    case Expect.getFailure expectation of
        Just { given, message } ->
            let
                givenElem =
                    if String.isEmpty given then
                        text ""
                    else
                        pre givenAttributes [ text ("Given " ++ given) ]
            in
                div []
                    [ givenElem
                    , pre messageAttributes [ text message ]
                    ]
                    |> Just

        Nothing ->
            Nothing


formatDuration : Time -> String
formatDuration time =
    toString time ++ " ms"
