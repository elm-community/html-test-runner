module Test.Runner.Html exposing (run, runWithOptions)

{-| HTML Runner

Runs tests in a browser and reports the results in the DOM. You can bring up
one of these tests in elm-reactor to have it run and show outputs.

@docs run, runWithOptions

-}

import Test exposing (Test)
import Test.Runner exposing (formatLabels)
import Expect exposing (Expectation)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Task
import Set exposing (Set)
import Test.Runner.Html.App
import String
import Random.Pcg as Random
import Time exposing (Time)


type alias TestId =
    Int


type alias Model =
    { available : Dict TestId (() -> ( List String, List Expectation ))
    , running : Set TestId
    , queue : List TestId
    , completed : List ( List String, List Expectation )
    , startTime : Time
    , finishTime : Maybe Time
    }


type Msg
    = Dispatch
    | Finish Time


viewLabels : List String -> List (Html a)
viewLabels =
    formatLabels (withColorChar '↓' "darkgray")
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


withoutEmptyStrings : List String -> List String
withoutEmptyStrings =
    List.filter ((/=) "")


withColorChar : Char -> String -> String -> Html a
withColorChar char textColor str =
    div [ style [ ( "color", textColor ) ] ]
        [ text (String.cons char (String.cons ' ' str)) ]


view : Model -> Html Msg
view model =
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
            List.filter (snd >> List.any ((/=) Expect.pass)) model.completed
    in
        div [ style [ ( "width", "960px" ), ( "margin", "auto 40px" ), ( "font-family", "verdana, sans-serif" ) ] ]
            [ summary
            , ol [ class "results", resultsStyle ] (List.map viewFailures failures)
            ]


resultsStyle : Html.Attribute a
resultsStyle =
    style [ ( "font-size", "14px" ), ( "line-height", "1.3" ), ( "font-family", "Menlo, Consolas, \"Fira Mono\", \"DejaVu Sans Mono\", \"Liberation Monospace\", \"Liberation Mono\", Monaco, \"Lucida Console\", \"Courier New\", monospace" ) ]


never : Never -> a
never a =
    never a


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


warn : String -> a -> a
warn str result =
    let
        _ =
            Debug.log str
    in
        result


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Finish time ->
            case model.finishTime of
                Nothing ->
                    ( { model | finishTime = Just time }, Cmd.none )

                Just _ ->
                    ( model, Cmd.none )
                        |> warn "Attempted to Finish more than once!"

        Dispatch ->
            case model.queue of
                [] ->
                    ( model, Task.perform never Finish Time.now )

                testId :: newQueue ->
                    case Dict.get testId model.available of
                        Nothing ->
                            ( model, Cmd.none )
                                |> warn ("Could not find testId " ++ toString testId)

                        Just run ->
                            let
                                completed =
                                    model.completed ++ [ run () ]

                                available =
                                    Dict.remove testId model.available

                                newModel =
                                    { model
                                        | completed = completed
                                        , available = available
                                        , queue = newQueue
                                    }

                                {- Dispatch as a Cmd so as to yield to the UI
                                   thread in between test executions.
                                -}
                            in
                                ( newModel, dispatch )


dispatch : Cmd Msg
dispatch =
    Task.succeed Dispatch
        |> Task.perform identity identity


init : Time -> List (() -> ( List String, List Expectation )) -> ( Model, Cmd Msg )
init startTime thunks =
    let
        indexedThunks : List ( TestId, () -> ( List String, List Expectation ) )
        indexedThunks =
            List.indexedMap (,) thunks

        model =
            { available = Dict.fromList indexedThunks
            , running = Set.empty
            , queue = List.map fst indexedThunks
            , completed = []
            , startTime = startTime
            , finishTime = Nothing
            }
    in
        ( model, dispatch )


formatDuration : Time -> String
formatDuration time =
    toString time ++ " ms"


{-| Run the test and report the results.

Fuzz tests use a default run count of 100, and an initial seed based on the
system time when the test runs begin.
-}
run : Test -> Program Never
run =
    runWithOptions Nothing Nothing


{-| Run the test using the provided options. If `Nothing` is provided for either
`runs` or `seed`, it will fall back on the options used in [`run`](#run).
-}
runWithOptions : Maybe Int -> Maybe Random.Seed -> Test -> Program Never
runWithOptions runs seed =
    Test.Runner.Html.App.run
        { runs = runs
        , seed = seed
        }
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
