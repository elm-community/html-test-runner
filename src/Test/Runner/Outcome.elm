module Test.Runner.Outcome
    exposing
        ( Failure
        , Outcome
        , Reason(..)
        , Status(..)
        , fromTest
        , step
        )

import Expect exposing (Expectation)
import Random.Pcg as Random
import Test exposing (Test)
import Test.Runner exposing (Runner)


type Outcome
    = Outcome Internals


type alias Internals =
    { passed : Int
    , failures : List Failure
    , todos : List Failure
    , runners : List Runner
    , autoFail : Maybe Reason
    }


type Status
    = Running
        { passed : Int
        , remaining : Int
        , failures : List Failure
        , next : Outcome
        }
    | Pass Int
    | Fail Int (List Failure)
    | Todo Int (List Failure)
    | AutoFail Int Reason


type Reason
    = Skip
      -- | Custom String
    | Only


type alias Failure =
    ( List String, List { given : Maybe String, message : String } )


fromTest : Int -> Random.Seed -> Test -> Outcome
fromTest runs seed test =
    let
        new runners autoFail =
            Outcome
                { passed = 0
                , failures = []
                , todos = []
                , runners = runners
                , autoFail = autoFail
                }
    in
    case Test.Runner.fromTest runs seed test of
        Test.Runner.Plain runners ->
            new runners Nothing

        Test.Runner.Only runners ->
            new runners (Just Only)

        Test.Runner.Skipping runners ->
            new runners (Just Skip)

        Test.Runner.Invalid _ ->
            Debug.crash "Invalid"


step : Outcome -> Status
step (Outcome internals) =
    case
        ( internals.autoFail
        , internals.todos
        , internals.failures
        , internals.runners
        )
    of
        ( Nothing, [], [], [] ) ->
            Pass internals.passed

        ( Nothing, todos, [], [] ) ->
            Todo internals.passed todos

        ( Just reason, _, [], [] ) ->
            AutoFail internals.passed reason

        ( _, _, failures, [] ) ->
            Fail internals.passed failures

        ( _, _, _, next :: queue ) ->
            next.run ()
                |> fromExpectation { internals | runners = queue } next.labels


fromExpectation : Internals -> List String -> List Expect.Expectation -> Status
fromExpectation internals labels expectations =
    let
        ( todos, failures ) =
            List.foldr partition ( [], [] ) expectations

        partition e old =
            case ( Test.Runner.isTodo e, Test.Runner.getFailure e ) of
                ( True, Just result ) ->
                    Tuple.mapFirst ((::) result) old

                ( False, Just result ) ->
                    Tuple.mapSecond ((::) result) old

                ( _, Nothing ) ->
                    old
    in
    if List.isEmpty failures && List.isEmpty todos then
        toRunning
            { internals
                | passed = internals.passed + 1
            }
    else if List.isEmpty failures then
        toRunning
            { internals
                | todos = internals.todos ++ [ ( labels, todos ) ]
            }
    else
        toRunning
            { internals
                | failures = internals.failures ++ [ ( labels, failures ) ]
            }


toRunning : Internals -> Status
toRunning internals =
    Running
        { passed = internals.passed
        , remaining = List.length internals.runners
        , failures = internals.failures
        , next = Outcome internals
        }
