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
    | Running (List (() -> ( List String, List Expectation ))) State
    | Finished Time State


type alias State =
    { completed : List ( List String, List Expectation )
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
            { completed = []
            , startTime = time
            , finishTime = Nothing
            }
    in
        ( Running thunks state, dispatch )


init : Maybe Int -> Maybe Random.Seed -> Test -> ( Model, Cmd Msg )
init maybeRuns seed test =
    let
        runs =
            Maybe.withDefault defaultRunCount maybeRuns
    in
        ( NotStarted seed runs test
        , Task.perform Start Time.now
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Start time, NotStarted Nothing runs test ) ->
            start runs time test (Random.initialSeed <| floor time)

        ( Start time, NotStarted (Just seed) runs test ) ->
            start runs time test seed

        ( Finish time, Running _ state ) ->
            ( Finished time state, Cmd.none )

        ( Dispatch, Running [] state ) ->
            ( Running [] state, Task.perform Finish Time.now )

        ( Dispatch, Running (run :: newQueue) state ) ->
            ( Running newQueue
                { state | completed = state.completed ++ [ run () ] }
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

        Running queue state ->
            View.Running
                { completed = List.length state.completed
                , remaining = List.length queue
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
            [ \() -> ( labels, Test.Runner.run runnable )
            ]

        Labeled label subRunner ->
            toThunks (label :: labels) subRunner

        Batch runners ->
            List.concatMap (toThunks labels) runners


defaultRunCount : Int
defaultRunCount =
    100
