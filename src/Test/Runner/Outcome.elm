module Test.Runner.Outcome
    exposing
        ( Outcome(..)
        , Failure
        , mapFailure
        , countFailures
        , run
        )

import Test.Runner exposing (Runner)
import Expect exposing (Expectation)


type alias Failure =
    ( List String, List { given : Maybe String, message : String } )


type Outcome
    = Pass
    | Todo (List Failure)
    | Fail (List Failure)


mapFailure : (Failure -> a) -> Outcome -> List a
mapFailure f outcome =
    case outcome of
        Pass ->
            []

        Todo failures ->
            List.map f failures

        Fail failures ->
            List.map f failures


countFailures : Outcome -> Int
countFailures outcome =
    case outcome of
        Pass ->
            0

        Todo failures ->
            List.length failures

        Fail failures ->
            List.length failures


run : Runner -> Outcome -> Outcome
run runner =
    runner.run ()
        |> fromExpectation runner.labels
        |> append


fromExpectation : List String -> List Expect.Expectation -> Outcome
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
            Todo [ ( labels, todos ) ]
        else
            Fail [ ( labels, failures ) ]


append : Outcome -> Outcome -> Outcome
append new old =
    case ( new, old ) of
        ( Pass, _ ) ->
            old

        ( _, Pass ) ->
            new

        ( Fail _, Todo _ ) ->
            new

        ( Todo _, Fail _ ) ->
            old

        ( Fail newMessages, Fail oldMessages ) ->
            Fail (oldMessages ++ newMessages)

        ( Todo newMessages, Todo oldMessages ) ->
            Todo (oldMessages ++ newMessages)
