module View exposing (view)

import Color exposing (rgb)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import String
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition
import Test.Runner.Exploration as Runner
import Test.Runner.Html.View as View
import Time exposing (Time)


type Styles
    = None
    | TestPane
    | TestResult
    | Main
    | TopBorder
    | TestHeader
    | TestSummary
    | TestPanel
    | Tests
    | ColoredText TestColor
    | GivenCode


type TestColor
    = FailColor
    | SucceedColor
    | TodoColor


palette =
    { primary = rgb 41 60 75
    , secondary = rgb 84 84 84 -- gray color on elm blog is rgb 221 221 221 but it doesn't meet accessibility standards for contrast http://webaim.org/resources/contrastchecker/
    , accent = rgb 96 181 204
    , background = rgb 255 255 255
    }


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None []
        , style TestPane
            [ Color.text palette.primary
            , Color.background palette.background
            , Color.border palette.secondary
            ]
        , style TestResult
            []
        , style Main
            [ Color.text (rgb 41 60 75)
            , Font.typeface [ "Source Sans Pro", "Trebuchet MS", "Lucida Grande", "Bitstream Vera Sans", "Helvetica Neue", "sans-serif" ]
            ]
        , style TopBorder
            [ Color.background palette.accent
            , paddingHint 4
            ]
        , style TestHeader
            [ Color.text Color.red
            , Font.size 20
            ]
        , style TestSummary
            [ paddingHint 2 ]
        , style TestPanel
            []
        , style Tests []
        , style (ColoredText FailColor) [ Color.text Color.red ]
        , style (ColoredText SucceedColor) [ Color.text Color.green ]
        , style (ColoredText TodoColor) [ Color.text Color.darkYellow ]
        , style GivenCode [ Color.text Color.gray ]
        ]



--
-- resultsStyle : Html.Attribute a
-- resultsStyle =
--     style
--         [ ( "font-size", "14px" )
--         , ( "line-height", "1.3" )
--         , ( "font-family", "Menlo, Consolas, \"Fira Mono\", \"DejaVu Sans Mono\", \"Liberation Monospace\", \"Liberation Mono\", Monaco, \"Lucida Console\", \"Courier New\", monospace" )
--         ]
-- messageAttributes : List (Html.Attribute a)
-- messageAttributes =
--     [ width 80
--     , style
--         [ ( "margin-left", "32px" )
--         , ( "margin-bottom", "40px" )
--         , ( "font-size", "inherit" )
--         , ( "font-family", "inherit" )
--         ]
--     ]
--


appContainer tests =
    column Main
        [ height (fill 1) ]
        [ topBorder
        , row None
            [ height (fill 1)
            , width (fill 1)
            ]
            [ tests ]
        ]


topBorder =
    el TopBorder [] Element.empty


view : View.Model -> Html a
view model =
    Element.viewport stylesheet <|
        appContainer <|
            case model of
                Nothing ->
                    "Loading Tests..."
                        |> text
                        |> el TestHeader []
                        |> header
                        |> summarize []

                Just ( duration, Runner.Pass passed ) ->
                    ( SucceedColor, "Test Run Passed" )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.Todo passed failures ) ->
                    ( TodoColor, "Test Run Incomplete: TODO's remaining" )
                        |> finished duration passed failures
                        |> summarize failures

                Just ( duration, Runner.AutoFail passed Runner.Only ) ->
                    ( TodoColor, "Test Run Incomplete: Test.only was used" )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.AutoFail passed Runner.Skip ) ->
                    ( TodoColor, "Test Run Incomplete: Test.skip was used" )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.AutoFail passed (Runner.Custom reason) ) ->
                    ( TodoColor, "Test Run Incomplete: " ++ reason )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.Fail passed failures ) ->
                    ( FailColor, "Test Run Failed" )
                        |> finished duration passed failures
                        |> summarize failures

                Just ( duration, Runner.Running { passed, failures, remaining } ) ->
                    running (passed + List.length failures) remaining
                        |> summarize failures


running : Int -> Int -> Element Styles variations msg
running completed remaining =
    column None
        []
        [ header <| el TestHeader [] (text "Running Tests...")
        , row None [] [ text (toString completed ++ " completed") ]
        , row None [] [ text (toString remaining ++ " remaining") ]
        ]


finished : Time -> Int -> List a -> ( TestColor, String ) -> Element Styles variation msg
finished duration passed failures ( headlineColor, headlineText ) =
    column None
        [ spacing 10, padding 10 ]
        [ row TestHeader [] [ header (text headlineText) ]
        , row None
            []
            [ grid TestResult
                { rows = [], columns = [] }
                [ spacing 10 ]
                [ area { start = ( 0, 0 ), width = 1, height = 1 } (bold "Duration")
                , area { start = ( 1, 0 ), width = 1, height = 1 } (text (formatDuration duration))
                , area { start = ( 0, 1 ), width = 1, height = 1 } (bold "Passed")
                , area { start = ( 1, 1 ), width = 1, height = 1 } (text (toString passed))
                , area { start = ( 0, 2 ), width = 1, height = 1 } (bold "Failed")
                , area { start = ( 1, 2 ), width = 1, height = 1 } (text (toString (List.length failures)))
                ]
            ]
        ]


summarize : List Runner.Failure -> Element Styles variation msg -> Element Styles variation msg
summarize failures summary =
    column TestPanel
        []
        [ row None [] [ summary ]
        , row None [] [ viewFailures failures ]
        ]


viewFailures failures =
    list Ordered
        None
        []
        (List.map viewFailure failures)


type Ordered
    = Ordered
    | Unordered


list : Ordered -> Styles -> List (Element.Attribute variation msg) -> List (Element Styles variation msg) -> Element Styles variation msg
list ordered style attrs els =
    let
        elements =
            List.map (node "li") els
    in
    case ordered of
        Ordered ->
            node "ol" (column None [] elements)

        Unordered ->
            node "ul" (column None [] elements)


viewFailure : Runner.Failure -> Element Styles variations msg
viewFailure failure =
    let
        ( labels, expectations ) =
            Runner.formatFailure
                (withTestColor '↓' TodoColor)
                (withTestColor '✗' FailColor)
                failure

        inContext { given, message } =
            column None
                []
                [ Maybe.withDefault Element.empty (Maybe.map viewGiven given)
                , code None [] message
                ]
    in
    row None
        []
        (labels ++ List.map inContext expectations)


code style attrs str =
    node "pre" (el style attrs (text str))


viewGiven : String -> Element Styles variations msg
viewGiven value =
    code GivenCode [] ("Given " ++ value)


withTestColor : Char -> TestColor -> String -> Element Styles variation msg
withTestColor char textColor str =
    column (ColoredText textColor)
        []
        [ text (String.cons char (String.cons ' ' str)) ]


formatDuration : Time -> String
formatDuration time =
    toString time ++ " ms"
