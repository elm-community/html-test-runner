module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Test.Runner
import Test.Runner.Html.View as View
import Test.Runner.Outcome as Outcome
import Time exposing (Time)


view : View.Model -> Html a
view model =
    case model of
        View.NotStarted ->
            text ""

        View.Running { completed, remaining, status } ->
            running completed remaining status

        View.Finished { duration, passed, status } ->
            finished duration passed status


running : Int -> Int -> Outcome.Status -> Html a
running completed remaining status =
    summarize status <|
        div []
            [ h2 [] [ text "Running Tests..." ]
            , div [] [ text (toString completed ++ " completed") ]
            , div [] [ text (toString remaining ++ " remaining") ]
            ]


finished : Time -> Int -> Outcome.Status -> Html a
finished duration passed status =
    let
        ( headlineColor, headlineText ) =
            header status

        thStyle =
            [ ( "text-align", "left" ), ( "padding-right", "10px" ) ]
    in
    summarize status <|
        div []
            [ h2 [ style [ ( "color", headlineColor ) ] ]
                [ text headlineText ]
            , table []
                [ tbody []
                    [ tr []
                        [ th [ style thStyle ]
                            [ text "Duration" ]
                        , td []
                            [ text (formatDuration duration) ]
                        ]
                    , tr []
                        [ th [ style thStyle ]
                            [ text "Passed" ]
                        , td []
                            [ text (toString passed) ]
                        ]
                    , tr []
                        [ th [ style thStyle ]
                            [ text "Failed" ]
                        , td []
                            [ text <|
                                toString <|
                                    case status of
                                        Outcome.Pass ->
                                            0

                                        Outcome.Fail _ failures ->
                                            List.length failures
                            ]
                        ]
                    ]
                ]
            ]


header : Outcome.Status -> ( String, String )
header status =
    case status of
        Outcome.Pass ->
            ( palette.green
            , "Test Run Passed"
            )

        Outcome.Fail Outcome.Normal _ ->
            ( palette.red
            , "Test Run Failed"
            )

        Outcome.Fail Outcome.Todo failures ->
            ( palette.yellow
            , "Test Run Incomplete: "
                ++ toString (List.length failures)
                ++ " TODOs remaining"
            )

        Outcome.Fail Outcome.Only _ ->
            ( palette.yellow
            , "Test Run Incomplete: Test.only was used"
            )

        Outcome.Fail Outcome.Skip _ ->
            ( palette.yellow
            , "Test Run Incomplete: Test.skip was used"
            )


summarize : Outcome.Status -> Html a -> Html a
summarize status content =
    div
        [ style
            [ ( "width", "960px" )
            , ( "margin", "auto 40px" )
            , ( "font-family", "verdana, sans-serif" )
            ]
        ]
        [ content
        , ol [ class "results", resultsStyle ] <|
            case status of
                Outcome.Pass ->
                    []

                Outcome.Fail _ failures ->
                    List.map viewFailure failures
        ]


viewFailure : Outcome.Failure -> Html a
viewFailure ( labels, expectations ) =
    let
        inContext { given, message } =
            div []
                [ case given of
                    Nothing ->
                        text ""

                    Just value ->
                        pre givenAttributes [ text ("Given " ++ value) ]
                , pre messageAttributes [ text message ]
                ]
    in
    li
        [ style [ ( "margin", "40px 0" ) ] ]
        (viewLabels labels ++ List.map inContext expectations)


viewLabels : List String -> List (Html a)
viewLabels =
    Test.Runner.formatLabels
        (withColorChar '↓' palette.gray)
        (withColorChar '✗' palette.red)


givenAttributes : List (Html.Attribute a)
givenAttributes =
    [ width 80
    , style
        [ ( "margin-bottom", "24px" )
        , ( "color", palette.gray )
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
    style
        [ ( "font-size", "14px" )
        , ( "line-height", "1.3" )
        , ( "font-family", "Menlo, Consolas, \"Fira Mono\", \"DejaVu Sans Mono\", \"Liberation Monospace\", \"Liberation Mono\", Monaco, \"Lucida Console\", \"Courier New\", monospace" )
        ]


formatDuration : Time -> String
formatDuration time =
    toString time ++ " ms"


palette :
    { green : String
    , red : String
    , yellow : String
    , gray : String
    }
palette =
    { green = "darkgreen"
    , red = "hsla(3, 100%, 40%, 1.0)"
    , yellow = "goldenrod"
    , gray = "darkgray"
    }
