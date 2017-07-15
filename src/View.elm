module View exposing (view)

import Color exposing (Color, rgb)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import String
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
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


type alias Palette =
    { primary : Color
    , secondary : Color
    , accent : Color
    , background : Color
    }


palette : Palette
palette =
    { primary = rgb 41 60 75
    , secondary = rgb 84 84 84

    -- gray color on elm blog is rgb 221 221 221 but it doesn't meet
    -- accessibility standards for contrast http://webaim.org/resources/contrastchecker/
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
        , style TestPanel []
        , style Tests []
        , style (ColoredText FailColor) [ Color.text Color.red ]
        , style (ColoredText SucceedColor) [ Color.text Color.green ]
        , style (ColoredText TodoColor) [ Color.text Color.darkYellow ]
        , style GivenCode [ Color.text Color.gray ]
        ]


appContainer : Element Styles variations msg -> Element Styles variations msg
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


topBorder : Element Styles variations msg
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

                Just ( duration, Runner.Incomplete passed Runner.Only ) ->
                    ( TodoColor, "Test Run Incomplete: Test.only was used" )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.Incomplete passed Runner.Skip ) ->
                    ( TodoColor, "Test Run Incomplete: Test.skip was used" )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.Incomplete passed (Runner.Custom reason) ) ->
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
            [ table TestResult
                [ spacing 10 ]
                [ [ bold "Duration", bold "Passed", bold "Failed" ]
                , [ text (formatDuration duration)
                  , text (toString passed)
                  , text (toString (List.length failures))
                  ]
                ]
            ]
        ]


summarize : List Runner.Failure -> Element Styles variation msg -> Element Styles variation msg
summarize failures summary =
    column TestPanel
        [ padding 10, maxWidth (px 700) ]
        [ wrappedRow None [] [ summary ]
        , wrappedRow None [] [ viewFailures failures ]
        ]


viewFailures : List Runner.Failure -> Element Styles variation msg
viewFailures failures =
    list Ordered
        None
        []
        (List.map viewFailure failures)


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
                [ wrappedRow None [] [ Maybe.withDefault Element.empty (Maybe.map viewGiven given) ]
                , wrappedRow None [] [ code None [ inlineStyle [ ( "white-space", "pre-wrap" ) ] ] message ]
                ]
    in
    el None
        [ inlineStyle [ ( "display", "list-item" ), ( "margin", "10px" ), ( "padding", "10px" ) ]
        ]
        (column None
            [ spacing 5 ]
            (labels ++ List.map inContext expectations)
        )



-- HELPERS


viewGiven : String -> Element Styles variations msg
viewGiven value =
    code GivenCode [ inlineStyle [ ( "white-space", "pre-wrap" ) ] ] ("Given " ++ value)


withTestColor : Char -> TestColor -> String -> Element Styles variation msg
withTestColor char textColor str =
    column (ColoredText textColor)
        []
        [ text (String.cons char (String.cons ' ' str)) ]


formatDuration : Time -> String
formatDuration time =
    toString time ++ " ms"



-- STYLE ELEMENTS HELPERS


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
            node "ol" (column None [ inlineStyle [ ( "display", "block" ), ( "margin", "10px" ), ( "padding", "10px" ) ] ] elements)

        Unordered ->
            node "ul" (column None [ inlineStyle [ ( "display", "block" ), ( "margin", "10px" ), ( "padding", "10px" ) ] ] elements)


code : style -> List (Element.Attribute variations msg) -> String -> Element style variations msg
code style attrs str =
    node "pre" <| el style attrs (text str)
