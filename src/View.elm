module View exposing (view)

import Color exposing (Color, rgb)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import String
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Test.Runner.Exploration as Runner
import Test.Runner.Html.View as View
import Time exposing (Time)


type Styles
    = None
    | Main
    | Results
    | Test
    | Header Palette
    | Description Palette
    | Given


type Palette
    = Primary
    | Secondary
    | Accent
    | Background
    | Good
    | Bad
    | Warning


color : Palette -> Color
color palette =
    case palette of
        Primary ->
            rgb 41 60 75

        Secondary ->
            -- gray color on elm blog is rgb 221 221 221 but it doesn't meet
            -- accessibility standards for contrast http://webaim.org/resources/contrastchecker/
            rgb 84 84 84

        Accent ->
            rgb 96 181 204

        Background ->
            rgb 255 255 255

        Good ->
            rgb 0 100 0

        Bad ->
            rgb 179 0 0

        Warning ->
            rgb 122 67 0


withColor :
    (Palette -> class)
    -> List (Property class variation)
    -> List (Style class variation)
withColor toStyle attributes =
    let
        withColorHelp p =
            style
                (toStyle p)
                (Color.text (color p) :: attributes)
    in
    List.map withColorHelp
        [ Primary
        , Secondary
        , Accent
        , Background
        , Good
        , Bad
        , Warning
        ]


stylesheet : StyleSheet Styles variation
stylesheet =
    [ [ style None []
      , style Results
            []
      , style Main
            [ Color.text (rgb 41 60 75)
            , Color.border (color Accent)
            , Font.typeface
                [ "Source Sans Pro"
                , "Trebuchet MS"
                , "Lucida Grande"
                , "Bitstream Vera Sans"
                , "Helvetica Neue"
                , "sans-serif"
                ]
            , Border.top 8
            , Style.paddingHint 20
            ]
      , style Test []
      , style Given
            [ Color.text (color Primary) ]
      ]
    , withColor Description []
    , withColor Header [ Font.size 24, Font.bold, paddingBottomHint 24 ]
    ]
        |> List.concat
        |> Style.stylesheet


appContainer : Element Styles variations msg -> Element Styles variations msg
appContainer tests =
    column Main
        [ height (fill 1) ]
        [ row None
            [ height (fill 1)
            , width (fill 1)
            ]
            [ tests ]
        ]


view : View.Model -> Html a
view model =
    Element.viewport stylesheet <|
        appContainer <|
            case model of
                Nothing ->
                    "Loading Tests..."
                        |> text
                        |> el (Header Primary) []
                        |> header
                        |> summarize []

                Just ( duration, Runner.Pass passed ) ->
                    ( Good, "Test Run Passed" )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.Todo passed failures ) ->
                    ( Warning, "Test Run Incomplete: TODO's remaining" )
                        |> finished duration passed failures
                        |> summarize failures

                Just ( duration, Runner.Incomplete passed Runner.Only ) ->
                    ( Warning, "Test Run Incomplete: Test.only was used" )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.Incomplete passed Runner.Skip ) ->
                    ( Warning, "Test Run Incomplete: Test.skip was used" )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.Incomplete passed (Runner.Custom reason) ) ->
                    ( Warning, "Test Run Incomplete: " ++ reason )
                        |> finished duration passed []
                        |> summarize []

                Just ( duration, Runner.Fail passed failures ) ->
                    ( Bad, "Test Run Failed" )
                        |> finished duration passed failures
                        |> summarize failures

                Just ( duration, Runner.Running { passed, failures, remaining } ) ->
                    running (passed + List.length failures) remaining
                        |> summarize failures


running : Int -> Int -> Element Styles variations msg
running completed remaining =
    column None
        []
        [ header <| el (Header Primary) [] (text "Running Tests...")
        , row None [] [ text (toString completed ++ " completed") ]
        , row None [] [ text (toString remaining ++ " remaining") ]
        ]


finished : Time -> Int -> List a -> ( Palette, String ) -> Element Styles variation msg
finished duration passed failures ( headlineColor, headlineText ) =
    column None
        []
        [ row (Header headlineColor) [] [ header (text headlineText) ]
        , row None
            []
            [ table Results
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
    column Test
        [ maxWidth (px 700) ]
        [ wrappedRow None [] [ summary ]
        , wrappedRow None [] [ viewFailures failures ]
        ]


viewFailures : List Runner.Failure -> Element Styles variation msg
viewFailures failures =
    List.map (viewFailure >> node "li") failures
        |> column None [ inlineStyle [ ( "display", "block" ), ( "margin", "10px" ), ( "padding", "10px" ) ] ]
        |> node "ol"


viewFailure : Runner.Failure -> Element Styles variations msg
viewFailure failure =
    let
        ( labels, expectations ) =
            Runner.formatFailure
                (withTestColor '↓' Secondary)
                (withTestColor '✗' Bad)
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


viewGiven : String -> Element Styles variations msg
viewGiven value =
    code Given [ inlineStyle [ ( "white-space", "pre-wrap" ) ] ] ("Given " ++ value)


withTestColor : Char -> Palette -> String -> Element Styles variation msg
withTestColor char textColor str =
    column (Description textColor)
        []
        [ text (String.cons char (String.cons ' ' str)) ]


formatDuration : Time -> String
formatDuration time =
    toString time ++ " ms"


code : style -> List (Element.Attribute variations msg) -> String -> Element style variations msg
code style attrs str =
    node "pre" <| el style attrs (text str)
