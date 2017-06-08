module Test.Runner.Html.App
    exposing
        ( Msg(..)
        , Model
        , init
        , update
        , present
        )

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
    { queue : List (() -> ( List String, List Expectation ))
    , completed : List ( List String, List Expectation )
    , startTime : Time
    , finishTime : Maybe Time
    }


{-| Dispatch as a Cmd so as to yield to the UI
    thread in between test executions.
-}
dispatch : Cmd Msg
dispatch =
    Task.succeed Dispatch
        |> Task.perform identity


start : Int -> Time -> Test -> Random.Seed -> ( Model, Cmd Msg )
start runs time test seed =
    let
        thunks =
            test
                |> Test.Runner.fromTest runs seed
                |> toThunks []

        state =
            { queue = thunks
            , completed = []
            , startTime = time
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Start time, NotStarted seed runs test ) ->
            Maybe.withDefault (Random.initialSeed <| floor time) seed
                |> start runs time test

        ( Finish time, Running state ) ->
            ( Finished time state, Cmd.none )

        ( Dispatch, Running state ) ->
            case state.queue of
                [] ->
                    ( Running state, Task.perform Finish Time.now )

                run :: newQueue ->
                    ( Running
                        { state
                            | queue = newQueue
                            , completed = state.completed ++ [ run () ]
                        }
                    , dispatch
                    )

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
                , remaining = List.length state.queue
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
