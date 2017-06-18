module Main exposing (..)

import Expect
import Test exposing (..)
import Test.Runner.Html
import Test.Runner.Html.App as App
import Test.Runner.Html.View as View
import Test.Runner.Outcome as Outcome
import Fixtures


suite : Test
suite =
    [ test "shows nothing on init" <|
        \_ ->
            init Fixtures.noTests
                |== View.NotStarted
    , test "no tests has one (failure) expectation" <|
        \_ ->
            init Fixtures.noTests
                |- App.Start 5
                |== View.Running
                        { completed = 0
                        , remaining = 1
                        , outcome = Outcome.Pass
                        }
    , test "fails when describe has no tests" <|
        \_ ->
            init Fixtures.noTests
                |- App.Start 5
                |- App.Dispatch
                |- App.Finish 10
                |== View.Finished
                        { duration = 5
                        , passed = 0
                        , outcome =
                            Outcome.Fail
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
                        }
    , test "passing one nested test" <|
        \_ ->
            init Fixtures.oneTest
                |- App.Start 5
                |- App.Dispatch
                |- App.Dispatch
                |- App.Finish 15
                |== View.Finished
                        { duration = 10
                        , passed = 1
                        , outcome = Outcome.Pass
                        }
    , test "increments test counter" <|
        \_ ->
            init Fixtures.twoTests
                |- App.Start 5
                |- App.Dispatch
                |== View.Running
                        { completed = 1
                        , remaining = 1
                        , outcome = Outcome.Pass
                        }
    , test "captures failures" <|
        \_ ->
            init Fixtures.twoTests
                |- App.Start 5
                |- App.Dispatch
                |- App.Dispatch
                |== View.Running
                        { completed = 2
                        , remaining = 0
                        , outcome =
                            Outcome.Fail
                                [ ( [ "two", "both" ]
                                  , [ { given = Nothing, message = "message" } ]
                                  )
                                ]
                        }
    , test "doesn't show todo with failure" <|
        \_ ->
            init Fixtures.todoWithFailingTest
                |- App.Start 5
                |- App.Dispatch
                |- App.Dispatch
                |== View.Running
                        { completed = 2
                        , remaining = 0
                        , outcome =
                            Outcome.Fail
                                [ ( [ "done", "todo then failing" ]
                                  , [ { given = Nothing, message = "just cause" } ]
                                  )
                                ]
                        }
    , test "shows todo with passing" <|
        \_ ->
            init Fixtures.todoWithPassingTest
                |- App.Start 5
                |- App.Dispatch
                |- App.Dispatch
                |== View.Running
                        { completed = 2
                        , remaining = 0
                        , outcome =
                            Outcome.Todo
                                [ ( [ "todo then passing" ]
                                  , [ { given = Nothing, message = "haven't done this yet" } ]
                                  )
                                ]
                        }
    ]
        |> describe "Test.Runner.Html.App"


main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run suite



-- HELPERS


init : (() -> Test) -> ( App.Model, Cmd App.Msg )
init f =
    App.init 100 Nothing (f ())


{-| Extract Model then update it.
-}
(|-) : ( App.Model, a ) -> App.Msg -> ( App.Model, Cmd App.Msg )
(|-) ( model, _ ) msg =
    App.update msg model
infixl 0 |-


{-| Shortcut for Expect.equal.
-}
(|==) : ( App.Model, b ) -> View.Model -> Expect.Expectation
(|==) ( appModel, _ ) viewModel =
    Expect.equal viewModel (App.present appModel)
infixl 0 |==
