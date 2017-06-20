module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Test.Runner
import Test.Runner.Exploration as Runner
import Test.Runner.Html.View as View
import Time exposing (Time)


view : View.Model -> Html a
view model =
    case model of
        Nothing ->
            h2 [] [ text "Loading Tests..." ]
                |> summarize []

        Just ( duration, Runner.Pass passed ) ->
            ( palette.green, "Test Run Passed" )
                |> finished duration passed []
                |> summarize []

        Just ( duration, Runner.Todo passed failures ) ->
            ( palette.yellow, "Test Run Incomplete: TODO's remaining" )
                |> finished duration passed failures
                |> summarize failures

        Just ( duration, Runner.AutoFail passed Runner.Only ) ->
            ( palette.yellow, "Test Run Incomplete: Test.only was used" )
                |> finished duration passed []
                |> summarize []

        Just ( duration, Runner.AutoFail passed Runner.Skip ) ->
            ( palette.yellow, "Test Run Incomplete: Test.skip was used" )
                |> finished duration passed []
                |> summarize []

        Just ( duration, Runner.AutoFail passed (Runner.Custom reason) ) ->
            ( palette.yellow, "Test Run Incomplete: " ++ reason )
                |> finished duration passed []
                |> summarize []

        Just ( duration, Runner.Fail passed failures ) ->
            ( palette.red, "Test Run Failed" )
                |> finished duration passed failures
                |> summarize failures

        Just ( duration, Runner.Running { passed, failures, remaining } ) ->
            running (passed + List.length failures) remaining
                |> summarize failures


running : Int -> Int -> Html a
running completed remaining =
    div []
        [ h2 [] [ text "Running Tests..." ]
        , div [] [ text (toString completed ++ " completed") ]
        , div [] [ text (toString remaining ++ " remaining") ]
        ]


finished : Time -> Int -> List a -> ( String, String ) -> Html b
finished duration passed failures ( headlineColor, headlineText ) =
    let
        thStyle =
            [ ( "text-align", "left" ), ( "padding-right", "10px" ) ]
    in
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
                    , td [] [ text (toString (List.length failures)) ]
                    ]
                ]
            ]
        ]


summarize : List Runner.Failure -> Html a -> Html a
summarize failures content =
    div
        [ style
            [ ( "width", "960px" )
            , ( "margin", "auto 40px" )
            , ( "font-family", "verdana, sans-serif" )
            ]
        ]
        [ content
        , ol
            [ class "results", resultsStyle ]
            (List.map viewFailure failures)
        ]


viewFailure : Runner.Failure -> Html a
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
