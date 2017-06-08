module View exposing (view)

import String
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Test.Runner
import Test.Runner.Html.View as View


view : View.Model -> Html a
view model =
    case model of
        View.NotStarted ->
            text ""

        View.Running { completed, remaining, failures } ->
            running completed remaining failures

        View.Finished { duration, passed, failures } ->
            finished duration passed failures


running : Int -> Int -> List View.FailGroup -> Html a
running completed remaining failures =
    summarize failures <|
        div []
            [ h2 [] [ text "Running Tests..." ]
            , div [] [ text (toString completed ++ " completed") ]
            , div [] [ text (toString remaining ++ " remaining") ]
            ]


finished : Time -> Int -> List View.FailGroup -> Html a
finished duration passed failures =
    let
        ( headlineColor, headlineText ) =
            if List.isEmpty failures then
                ( "darkgreen", "Test Run Passed" )
            else
                ( "hsla(3, 100%, 40%, 1.0)", "Test Run Failed" )

        thStyle =
            [ ( "text-align", "left" ), ( "padding-right", "10px" ) ]
    in
        summarize failures <|
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
                                [ text (toString (List.length failures)) ]
                            ]
                        ]
                    ]
                ]


summarize : List View.FailGroup -> Html a -> Html a
summarize failures content =
    div [ style [ ( "width", "960px" ), ( "margin", "auto 40px" ), ( "font-family", "verdana, sans-serif" ) ] ]
        [ content
        , ol [ class "results", resultsStyle ] (List.map viewFailGroup failures)
        ]


viewFailGroup : View.FailGroup -> Html a
viewFailGroup ( labels, expectations ) =
    let
        inContext { given, message } =
            div []
                [ if String.isEmpty given then
                    text ""
                  else
                    pre givenAttributes [ text ("Given " ++ given) ]
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
