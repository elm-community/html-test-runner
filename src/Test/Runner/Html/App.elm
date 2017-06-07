module Test.Runner.Html.App
    exposing
        ( Msg(..)
        , Model
        , init
        , update
        , present
        )

import Dict exposing (Dict)
import Set
import Time exposing (Time)
import Task
import Tuple
import Random.Pcg as Random
import Test exposing (Test)
import Test.Runner exposing (Runner(..))
import Test.Runner.Html.View as View
import Expect exposing (Expectation)


type Msg
    = Start Time
    | Dispatch
    | Finish Time


type Model
    = NotStarted (Maybe Random.Seed) Int Test
    | Started View.Model


warn : String -> a -> a
warn =
    Debug.log


{-| Dispatch as a Cmd so as to yield to the UI
    thread in between test executions.
-}
dispatch : Cmd Msg
dispatch =
    Task.succeed Dispatch
        |> Task.perform identity


start :
    Time
    -> List (() -> ( List String, List Expectation ))
    -> ( Model, Cmd Msg )
start startTime thunks =
    let
        indexedThunks : List ( Int, () -> ( List String, List Expectation ) )
        indexedThunks =
            List.indexedMap (,) thunks

        viewModel =
            { available = Dict.fromList indexedThunks
            , running = Set.empty
            , queue = List.map Tuple.first indexedThunks
            , completed = []
            , startTime = startTime
            , finishTime = Nothing
            }
    in
        ( Started viewModel, dispatch )


init : Maybe Int -> Maybe Random.Seed -> Test -> ( Model, Cmd Msg )
init maybeRuns seed test =
    let
        runs =
            Maybe.withDefault defaultRunCount maybeRuns

        getTime =
            Task.perform Start Time.now
    in
        ( NotStarted seed runs test
        , getTime
        )


timeToSeed : Time -> Random.Seed
timeToSeed =
    Random.initialSeed << floor


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Start time, NotStarted seed runs test ) ->
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
                    |> toThunks []
                    |> start time

        ( Finish time, Started viewModel ) ->
            case viewModel.finishTime of
                Nothing ->
                    ( Started { viewModel | finishTime = Just time }, Cmd.none )

                Just _ ->
                    ( Started viewModel, Cmd.none )
                        |> warn "Attempted to Finish more than once!"

        ( Dispatch, Started viewModel ) ->
            case viewModel.queue of
                [] ->
                    ( Started viewModel, Task.perform Finish Time.now )

                testId :: newQueue ->
                    case Dict.get testId viewModel.available of
                        Nothing ->
                            ( Started viewModel, Cmd.none )
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
                            in
                                ( Started newModel, dispatch )

        ( Start _, Started _ ) ->
            Debug.crash "Attempted to start twice!"

        ( _, NotStarted _ _ _ ) ->
            Debug.crash "Attempted to run a Msg pre-Start!"


present : Model -> Maybe View.Model
present model =
    case model of
        NotStarted _ _ _ ->
            Nothing

        Started viewModel ->
            Just viewModel


toThunks :
    List String
    -> Runner
    -> List (() -> ( List String, List Expectation ))
toThunks labels runner =
    case runner of
        Runnable runnable ->
            [ \() -> ( labels, Test.Runner.run runnable ) ]

        Labeled label subRunner ->
            toThunks (label :: labels) subRunner

        Batch runners ->
            List.concatMap (toThunks labels) runners


defaultRunCount : Int
defaultRunCount =
    100
