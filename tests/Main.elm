module Main exposing (..)

import Expect
import Fixtures
import Test exposing (..)
import Test.Runner.Html
import Test.Runner.Html.App as App
import Test.Runner.Html.View as View
import Test.Runner.Outcome as Outcome


suite : Test
suite =
    [ test "shows nothing on init" <|
        \_ ->
            init Fixtures.noTests
                |== Nothing
    , test "fails when describe has no tests" <|
        \_ ->
            init Fixtures.noTests
                |- App.update (App.Dispatch 5)
                |- App.update (App.Dispatch 15)
                |== Just
                        ( 10
                        , Outcome.Fail 0
                            [ ( []
                              , [ { given =
                                        Nothing
                                  , message =
                                        "This `describe \"nothing\"` "
                                            ++ "has no tests in it. "
                                            ++ "Let's give it some!"
                                  }
                                ]
                              )
                            ]
                        )
    , test "passing one nested test" <|
        \_ ->
            init Fixtures.oneTest
                |- App.update (App.Dispatch 5)
                |- App.update (App.Dispatch 15)
                |== Just ( 10, Outcome.Pass 1 )
    , test "increments test counter" <|
        \_ ->
            init Fixtures.twoTests
                |- App.update (App.Dispatch 5)
                |- expectRunning
                    { passed = 1
                    , remaining = 1
                    , failures = []
                    }
    , test "captures failures" <|
        \_ ->
            init Fixtures.twoTests
                |- App.update (App.Dispatch 5)
                |- App.update (App.Dispatch 6)
                |- expectRunning
                    { passed = 1
                    , remaining = 0
                    , failures =
                        [ ( [ "two", "both" ]
                          , [ { given = Nothing
                              , message = "message"
                              }
                            ]
                          )
                        ]
                    }
    , test "doesn't show todo with failure" <|
        \_ ->
            init Fixtures.todoWithFailingTest
                |- App.update (App.Dispatch 5)
                |- App.update (App.Dispatch 6)
                |- App.update (App.Dispatch 7)
                |== Just
                        ( 2
                        , Outcome.Fail 0
                            [ ( [ "done", "todo then failing" ]
                              , [ { given = Nothing
                                  , message = "just cause"
                                  }
                                ]
                              )
                            ]
                        )
    , test "shows todo with passing" <|
        \_ ->
            init Fixtures.todoWithPassingTest
                |- App.update (App.Dispatch 5)
                |- App.update (App.Dispatch 6)
                |- App.update (App.Dispatch 7)
                |== Just
                        ( 2
                        , Outcome.Todo 1
                            [ ( [ "todo then passing" ]
                              , [ { given = Nothing
                                  , message = "haven't done this yet"
                                  }
                                ]
                              )
                            ]
                        )
    , test "shows only in isolation" <|
        \_ ->
            init (Fixtures.oneTest >> only)
                |- App.update (App.Dispatch 5)
                |- App.update (App.Dispatch 99)
                |== Just ( 94, Outcome.AutoFail 1 Outcome.Only )
    , test "shows skip in isolation" <|
        \_ ->
            init (Fixtures.noTests >> skip)
                |- App.update (App.Dispatch 5)
                |- App.update (App.Dispatch 99)
                |== Just ( 94, Outcome.AutoFail 0 Outcome.Skip )
    ]
        |> describe "Test.Runner.Html.App"


main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run suite



-- HELPERS


init : (() -> Test) -> ( App.Model, Cmd App.Msg )
init f =
    App.init 100 Nothing (f ())


expectRunning :
    { passed : Int, remaining : Int, failures : List Outcome.Failure }
    -> App.Model
    -> Expect.Expectation
expectRunning expected model =
    case App.present model of
        Just ( _, Outcome.Running { passed, remaining, failures } ) ->
            Expect.equal
                ( expected.passed, expected.remaining, expected.failures )
                ( passed, remaining, failures )

        _ ->
            "Expected Outcome.Running "
                ++ toString expected
                ++ " but got "
                ++ toString model
                |> Expect.fail


(|-) : ( a, b ) -> (a -> c) -> c
(|-) ( a, _ ) f =
    f a
infixl 0 |-


(|==) : ( App.Model, b ) -> View.Model -> Expect.Expectation
(|==) ( appModel, _ ) viewModel =
    Expect.equal viewModel (App.present appModel)
infixl 0 |==
