module View exposing (view)

import Color exposing (rgb)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
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


elmBlue =
    rgb 96 181 204


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None []
        , style TestPane
            [ Color.text Color.black
            , Color.background Color.white
            , Color.border Color.lightGrey
            ]
        , style TestResult
            []
        , style Main
            [ Color.text (rgb 41 60 75)
            , Font.typeface [ "Source Sans Pro", "Trebuchet MS", "Lucida Grande", "Bitstream Vera Sans", "Helvetica Neue", "sans-serif" ]
            ]
        , style TopBorder
            [ Color.background elmBlue
            , paddingHint 4
            ]
        , style TestHeader
            [ Color.text Color.red
            , Font.size 20
            ]
        ]


testFailures =
    [ { given = Just "A test", message = "A test does not pass" } ]


view _ =
    ( "#e61212", "Test Run Failed" )
        |> finished 200 0 testFailures
        -- |> summarize testFailures
        |> appContainer
        |> Element.viewport stylesheet


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



--
-- view : View.Model -> Html a
-- view model =
--     case model of
--         Nothing ->
--             h2 [] [ text "Loading Tests..." ]
--                 |> summarize []
--
--         Just ( duration, Runner.Pass passed ) ->
--             ( palette.green, "Test Run Passed" )
--                 |> finished duration passed []
--                 |> summarize []
--
--         Just ( duration, Runner.Todo passed failures ) ->
--             ( palette.yellow, "Test Run Incomplete: TODO's remaining" )
--                 |> finished duration passed failures
--                 |> summarize failures
--
--         Just ( duration, Runner.AutoFail passed Runner.Only ) ->
--             ( palette.yellow, "Test Run Incomplete: Test.only was used" )
--                 |> finished duration passed []
--                 |> summarize []
--
--         Just ( duration, Runner.AutoFail passed Runner.Skip ) ->
--             ( palette.yellow, "Test Run Incomplete: Test.skip was used" )
--                 |> finished duration passed []
--                 |> summarize []
--
--         Just ( duration, Runner.AutoFail passed (Runner.Custom reason) ) ->
--             ( palette.yellow, "Test Run Incomplete: " ++ reason )
--                 |> finished duration passed []
--                 |> summarize []
--
--         Just ( duration, Runner.Fail passed failures ) ->
--             ( palette.red, "Test Run Failed" )
--                 |> finished duration passed failures
--                 |> summarize failures
--
--         Just ( duration, Runner.Running { passed, failures, remaining } ) ->
--             running (passed + List.length failures) remaining
--                 |> summarize failures
--
-- running : Int -> Int -> Html a
-- running completed remaining =
--     div []
--         [ h2 [] [ text "Running Tests..." ]
--         , div [] [ text (toString completed ++ " completed") ]
--         , div [] [ text (toString remaining ++ " remaining") ]
--         ]
-- finished : Time -> Int -> List a -> ( String, String ) -> List (Element Styles variation msg)


finished duration passed failures ( headlineColor, headlineText ) =
    column None
        [ spacing 10, padding 10 ]
        [ row TestHeader [] [ header (text headlineText) ]
        , row None
            []
            [ grid TestResult
                { rows = [], columns = [] }
                [ spacing 10 ]
                [ --area { start = ( 0, 0 ), width = 2, height = 1 }
                  area { start = ( 0, 0 ), width = 1, height = 1 } (bold "Duration")
                , area { start = ( 1, 0 ), width = 1, height = 1 } (text (formatDuration duration))
                , area { start = ( 0, 1 ), width = 1, height = 1 } (bold "Passed")
                , area { start = ( 1, 1 ), width = 1, height = 1 } (text (toString passed))
                , area { start = ( 0, 2 ), width = 1, height = 1 } (bold "Failed")
                , area { start = ( 1, 2 ), width = 1, height = 1 } (text (toString (List.length failures)))
                ]
            ]
        ]



-- summarize : List a -> Element Styles variation msg -> Element Styles variation msg
-- summarize failures content =
--     content
-- div
--     [ style
--         [ ( "width", "960px" )
--         , ( "margin", "auto 40px" )
--         , ( "font-family", "verdana, sans-serif" )
--         ]
--     ]
--     [ content
--
--     -- , ol
--     --     [ class "results", resultsStyle ]
--     --     (List.map viewFailure failures)
--     ]
-- viewFailure : Runner.Failure -> Html a
-- viewFailure failure =
--     let
--         ( labels, expectations ) =
--             Runner.formatFailure
--                 (withColorChar '↓' palette.gray)
--                 (withColorChar '✗' palette.red)
--                 failure
--
--         inContext { given, message } =
--             div []
--                 [ Maybe.withDefault (text "") (Maybe.map viewGiven given)
--                 , pre messageAttributes [ text message ]
--                 ]
--     in
--     li
--         [ style [ ( "margin", "40px 0" ) ] ]
--         (labels ++ List.map inContext expectations)
--
--
-- viewGiven : String -> Html a
-- viewGiven value =
--     pre givenAttributes [ text ("Given " ++ value) ]
--
-- givenAttributes : List (Html.Attribute a)
-- givenAttributes =
--     [ width 80
--     , style
--         [ ( "margin-bottom", "24px" )
--         , ( "color", palette.gray )
--         , ( "font-size", "inherit" )
--         , ( "font-family", "inherit" )
--         ]
--     ]
--
--
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
-- withColorChar : Char -> String -> String -> Html a
-- withColorChar char textColor str =
--     div [ style [ ( "color", textColor ) ] ]
--         [ text (String.cons char (String.cons ' ' str)) ]
--
--
-- resultsStyle : Html.Attribute a
-- resultsStyle =
--     style
--         [ ( "font-size", "14px" )
--         , ( "line-height", "1.3" )
--         , ( "font-family", "Menlo, Consolas, \"Fira Mono\", \"DejaVu Sans Mono\", \"Liberation Monospace\", \"Liberation Mono\", Monaco, \"Lucida Console\", \"Courier New\", monospace" )
--         ]


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
