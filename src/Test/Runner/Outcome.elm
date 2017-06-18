module Test.Runner.Outcome
    exposing
        ( Outcome
        , Status(..)
        , Reason(..)
        , Failure
        , fromTest
        , status
        , remaining
        , passed
        , step
        )

import Test exposing (Test)
import Test.Runner exposing (Runner)
import Expect exposing (Expectation)
import Random.Pcg as Random


type Outcome
    = Outcome
        { passed : Int
        , runners : List Runner
        , status : Status
        }


type Status
    = Pass
    | Fail Reason (List Failure)


type Reason
    = Normal
    | Todo
    | Only
    | Skip


type alias Failure =
    ( List String, List { given : Maybe String, message : String } )


fromTest : Int -> Random.Seed -> Test -> Outcome
fromTest runs seed test =
    case Test.Runner.fromTest runs seed test of
        Test.Runner.Plain runners ->
            Outcome { passed = 0, runners = runners, status = Pass }

        Test.Runner.Only runners ->
            Outcome { passed = 0, runners = runners, status = Fail Only [] }

        Test.Runner.Skipping runners ->
            Outcome { passed = 0, runners = runners, status = Fail Skip [] }

        Test.Runner.Invalid _ ->
            Outcome { passed = 0, runners = [], status = Pass }


status : Outcome -> Status
status (Outcome outcome) =
    outcome.status


remaining : Outcome -> Int
remaining (Outcome outcome) =
    List.length outcome.runners


passed : Outcome -> Int
passed (Outcome outcome) =
    outcome.passed


step : Outcome -> Outcome
step (Outcome outcome) =
    case outcome.runners of
        [] ->
            Outcome outcome

        next :: queue ->
            next.run ()
                |> fromExpectation next.labels
                |> \status ->
                    Outcome
                        { runners = queue
                        , status = append outcome.status status
                        , passed =
                            if status == Pass then
                                1 + outcome.passed
                            else
                                outcome.passed
                        }


fromExpectation : List String -> List Expect.Expectation -> Status
fromExpectation labels expectations =
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
            Pass
        else if List.isEmpty failures then
            Fail Todo [ ( labels, todos ) ]
        else
            Fail Normal [ ( labels, failures ) ]


append : Status -> Status -> Status
append old new =
    case ( old, new ) of
        ( Pass, Pass ) ->
            Pass

        ( Pass, Fail _ _ ) ->
            new

        ( Fail _ _, Pass ) ->
            old

        ( Fail Normal oldMessages, Fail Normal newMessages ) ->
            Fail Normal (oldMessages ++ newMessages)

        ( Fail Todo oldMessages, Fail Todo newMessages ) ->
            Fail Todo (oldMessages ++ newMessages)

        ( Fail Only oldMessages, Fail Only newMessages ) ->
            Fail Only (oldMessages ++ newMessages)

        ( Fail Skip oldMessages, Fail Skip newMessages ) ->
            Fail Skip (oldMessages ++ newMessages)

        ( Fail _ _, Fail Normal _ ) ->
            new

        ( Fail _ _, Fail _ _ ) ->
            old
