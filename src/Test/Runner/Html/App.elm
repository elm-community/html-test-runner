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
    | Started
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
        ( Started state, dispatch )


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

        ( Finish time, Started state ) ->
            case state.finishTime of
                Nothing ->
                    ( Started { state | finishTime = Just time }, Cmd.none )

                Just _ ->
                    ( Started state, Cmd.none )
                        |> warn "Attempted to Finish more than once!"

        ( Dispatch, Started state ) ->
            case state.queue of
                [] ->
                    ( Started state, Task.perform Finish Time.now )

                testId :: newQueue ->
                    case Dict.get testId state.available of
                        Nothing ->
                            ( Started state, Cmd.none )
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
                                ( Started newModel, dispatch )

        ( Start _, Started _ ) ->
            Debug.crash "Attempted to start twice!"

        ( _, NotStarted _ _ _ ) ->
            Debug.crash "Attempted to run a Msg pre-Start!"


present : Model -> View.Model
present model =
    case model of
        NotStarted _ _ _ ->
            View.NotStarted

        Started state ->
            let
                failures =
                    formatFailures state.completed
            in
                case state.finishTime of
                    Just finishTime ->
                        View.Finished
                            { duration =
                                finishTime - state.startTime
                            , passed =
                                List.length state.completed - List.length failures
                            , failures =
                                failures
                            }

                    Nothing ->
                        View.Running
                            { completed =
                                List.length state.completed
                            , remaining =
                                List.length state.queue + Dict.size state.available
                            , failures =
                                failures
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
