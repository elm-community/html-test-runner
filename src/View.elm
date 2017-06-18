module View exposing (view)

import String
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Test.Runner
import Test.Runner.Html.View as View
import Test.Runner.Outcome as Outcome


view : View.Model -> Html a
view model =
    case model of
        View.NotStarted ->
            text ""

        View.Running { completed, remaining, outcome } ->
            running completed remaining outcome

        View.Finished { duration, passed, outcome } ->
            finished duration passed outcome


running : Int -> Int -> Outcome.Outcome -> Html a
running completed remaining outcome =
    summarize outcome <|
        div []
            [ h2 [] [ text "Running Tests..." ]
            , div [] [ text (toString completed ++ " completed") ]
            , div [] [ text (toString remaining ++ " remaining") ]
            ]


finished : Time -> Int -> Outcome.Outcome -> Html a
finished duration passed outcome =
    let
        ( headlineColor, headlineText ) =
            if outcome == Outcome.Pass then
                ( "darkgreen", "Outcome Run Passed" )
            else
                ( "hsla(3, 100%, 40%, 1.0)", "Outcome Run Failed" )

        thStyle =
            [ ( "text-align", "left" ), ( "padding-right", "10px" ) ]
    in
        summarize outcome <|
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
                                [ text <| toString <| Outcome.countFailures outcome
                                ]
                            ]
                        ]
                    ]
                ]


summarize : Outcome.Outcome -> Html a -> Html a
summarize outcome content =
    div [ style [ ( "width", "960px" ), ( "margin", "auto 40px" ), ( "font-family", "verdana, sans-serif" ) ] ]
        [ content
        , ol [ class "results", resultsStyle ] (Outcome.mapFailure viewFailGroup outcome)
        ]


viewFailGroup : Outcome.Failure -> Html a
viewFailGroup ( labels, expectations ) =
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


formatDuration : Time -> String
formatDuration time =
    toString time ++ " ms"
