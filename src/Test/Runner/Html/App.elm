module Test.Runner.Html.App
    exposing
        ( Msg(..)
        , Model
        , init
        , update
        , present
        )

import Dict exposing (Dict)
import Time exposing (Time)
import Task
import Tuple
import Random.Pcg as Random
import Test exposing (Test)
import Test.Runner exposing (Runner(..))
import Test.Runner.Html.View as View
import Expect exposing (Expectation, getFailure)


type Msg
    = Start Time
    | Dispatch
    | Finish Time


type Model
    = NotStarted (Maybe Random.Seed) Int Test
    | Running State
    | Finished Time State


type alias State =
    { available : Dict Int (() -> ( List String, List Expectation ))
    , queue : List Int
    , completed : List ( List String, List Expectation )
    , startTime : Time
    , finishTime : Maybe Time
    }


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
        indexedThunks =
            List.indexedMap (,) thunks

        state =
            { available = Dict.fromList indexedThunks
            , queue = List.map Tuple.first indexedThunks
            , completed = []
            , startTime = startTime
            , finishTime = Nothing
            }
    in
        ( Running state, dispatch )


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

        ( Finish time, Running state ) ->
            ( Finished time state, Cmd.none )

        ( Dispatch, Running state ) ->
            case state.queue of
                [] ->
                    ( Running state, Task.perform Finish Time.now )

                testId :: newQueue ->
                    case Dict.get testId state.available of
                        Nothing ->
                            ( Running state, Cmd.none )
                                |> warn ("Could not find testId " ++ toString testId)

                        Just run ->
                            let
                                completed =
                                    state.completed ++ [ run () ]

                                available =
                                    Dict.remove testId state.available

                                newModel =
                                    { state
                                        | completed = completed
                                        , available = available
                                        , queue = newQueue
                                    }
                            in
                                ( Running newModel, dispatch )

        ( Start _, _ ) ->
            Debug.crash "Attempted to start twice!"

        ( _, NotStarted _ _ _ ) ->
            Debug.crash "Attempted to run a Msg pre-Start!"

        ( Finish _, Finished _ _ ) ->
            Debug.crash "Attempted to Finish more than once!"

        ( Dispatch, Finished _ _ ) ->
            Debug.crash "Attempted to run tests after program finished!"


present : Model -> View.Model
present model =
    case model of
        NotStarted _ _ _ ->
            View.NotStarted

        Running state ->
            View.Running
                { completed = List.length state.completed
                , remaining = Dict.size state.available
                , failures = formatFailures state.completed
                }

        Finished finishTime state ->
            let
                failures =
                    formatFailures state.completed
            in
                View.Finished
                    { duration = finishTime - state.startTime
                    , passed = List.length state.completed - List.length failures
                    , failures = failures
                    }


formatFailures : List ( List String, List Expectation ) -> List View.FailGroup
formatFailures =
    List.filterMap <|
        \( labels, expectations ) ->
            case List.filterMap getFailure expectations of
                [] ->
                    Nothing

                failures ->
                    Just ( labels, failures )


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
