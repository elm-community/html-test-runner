module Test.Runner.Html exposing (TestProgram, run, runWithOptions)

{-| HTML Runner

Runs tests in a browser and reports the results in the DOM. You can bring up
one of these tests in elm-reactor to have it run and show outputs.

@docs run, runWithOptions, TestProgram

-}

import Dict exposing (Dict)
import Set
import Time exposing (Time)
import Task
import Tuple
import Html exposing (Html)
import Random.Pcg as Random
import Test exposing (Test)
import Test.Runner exposing (Runner(..))
import Test.Runner.Html.View as View
import Expect exposing (Expectation)


type Msg
    = Init Time
    | Dispatch
    | Finish Time


type Model
    = Uninitialized (Maybe Random.Seed) Int Test
    | Initialized View.Model


{-| A program which will run tests and report their results.
-}
type alias TestProgram =
    Program Never Model Msg


type alias SubUpdate msg model =
    msg -> model -> ( model, Cmd msg )


type alias RunnerOptions =
    { seed : Maybe Random.Seed
    , runs : Maybe Int
    }


warn : String -> a -> a
warn =
    Debug.log


dispatch : Cmd Msg
dispatch =
    Task.succeed Dispatch
        |> Task.perform identity


initialize : Time -> List (() -> ( List String, List Expectation )) -> ( Model, Cmd Msg )
initialize startTime thunks =
    let
        indexedThunks : List ( Int, () -> ( List String, List Expectation ) )
        indexedThunks =
            List.indexedMap (,) thunks

        model =
            { available = Dict.fromList indexedThunks
            , running = Set.empty
            , queue = List.map Tuple.first indexedThunks
            , completed = []
            , startTime = startTime
            , finishTime = Nothing
            }
    in
        ( Initialized model, dispatch )


{-| Run the test and report the results.

Fuzz tests use a default run count of 100, and an initial seed based on the
system time when the test runs begin.
-}
run : Test -> TestProgram
run =
    runWithOptions Nothing Nothing


{-| Run the test using the provided options. If `Nothing` is provided for either
`runs` or `seed`, it will fall back on the options used in [`run`](#run).
-}
runWithOptions : Maybe Int -> Maybe Random.Seed -> Test -> TestProgram
runWithOptions maybeRuns seed test =
    let
        runs =
            Maybe.withDefault defaultRunCount maybeRuns

        getTime =
            Task.perform Init Time.now
    in
        Html.program
            { init = ( Uninitialized seed runs test, getTime )
            , update = update
            , view = view
            , subscriptions = \_ -> Sub.none
            }


timeToSeed : Time -> Random.Seed
timeToSeed time =
    (0xFFFFFFFF * time)
        |> floor
        |> Random.initialSeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Init time, Uninitialized seed runs test ) ->
            let
                finalSeed =
                    case seed of
                        Just realSeed ->
                            realSeed

                        Nothing ->
                            timeToSeed time
            in
                test
                    |> Test.Runner.fromTest runs finalSeed
                    |> toThunks
                    |> initialize time

        ( Finish time, Initialized viewModel ) ->
            case viewModel.finishTime of
                Nothing ->
                    ( Initialized { viewModel | finishTime = Just time }, Cmd.none )

                Just _ ->
                    ( Initialized viewModel, Cmd.none )
                        |> warn "Attempted to Finish more than once!"

        ( Dispatch, Initialized viewModel ) ->
            case viewModel.queue of
                [] ->
                    ( Initialized viewModel, Task.perform Finish Time.now )

                testId :: newQueue ->
                    case Dict.get testId viewModel.available of
                        Nothing ->
                            ( Initialized viewModel, Cmd.none )
                                |> warn ("Could not find testId " ++ toString testId)

                        Just run ->
                            let
                                completed =
                                    viewModel.completed ++ [ run () ]

                                available =
                                    Dict.remove testId viewModel.available

                                newModel =
                                    { viewModel
                                        | completed = completed
                                        , available = available
                                        , queue = newQueue
                                    }

                                {- Dispatch as a Cmd so as to yield to the UI
                                   thread in between test executions.
                                -}
                            in
                                ( Initialized newModel, dispatch )

        ( Init _, Initialized _ ) ->
            Debug.crash "Attempted to init twice!"

        ( _, Uninitialized _ _ _ ) ->
            Debug.crash "Attempted to run a Msg pre-Init!"


view : Model -> Html Msg
view model =
    case model of
        Uninitialized _ _ _ ->
            View.uninitialized

        Initialized viewModel ->
            View.initialized viewModel


toThunks : Runner -> List (() -> ( List String, List Expectation ))
toThunks =
    toThunksHelp []


toThunksHelp : List String -> Runner -> List (() -> ( List String, List Expectation ))
toThunksHelp labels runner =
    case runner of
        Runnable runnable ->
            [ \() -> ( labels, Test.Runner.run runnable ) ]

        Labeled label subRunner ->
            toThunksHelp (label :: labels) subRunner

        Batch runners ->
            List.concatMap (toThunksHelp labels) runners


defaultRunCount : Int
defaultRunCount =
    100
