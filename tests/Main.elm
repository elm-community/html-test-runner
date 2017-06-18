module Main exposing (..)

import Dict
import Set
import Expect
import Test exposing (..)
import Test.Runner.Html
import Test.Runner.Html.App as App
import Test.Runner.Html.View as View
import Test.Runner.Outcome as Outcome


-- FIXTURES


type alias Fixture =
    () -> Test


noTests : Fixture
noTests () =
    describe "nothing" []


todoWithFailingTest : Fixture
todoWithFailingTest () =
    describe "todo then passing"
        [ test "done" (\_ -> Expect.fail "just cause")
        , todo "haven't done this yet"
        ]


oneTest : Fixture
oneTest () =
    describe "a"
        [ describe "very"
            [ describe "nested"
                [ test "test" (\_ -> Expect.equal 1 1) ]
            ]
        ]


twoTests : Fixture
twoTests () =
    describe "both"
        [ test "one" (\_ -> Expect.pass)
        , test "two" (\_ -> Expect.fail "message")
        ]



-- REAL TESTS


suite : Test
suite =
    [ test "shows nothing on init" <|
        \_ ->
            init noTests
                |== View.NotStarted
    , test "no tests has one (failure) expectation" <|
        \_ ->
            init noTests
                |- App.update (App.Start 5)
                |== View.Running
                        { completed = 0
                        , remaining = 1
                        , outcome = Outcome.Pass
                        }
    , test "fails when describe has no tests" <|
        \_ ->
            init noTests
                |- App.update (App.Start 5)
                |- App.update App.Dispatch
                |- App.update (App.Finish 10)
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
            init oneTest
                |- App.update (App.Start 5)
                |- App.update App.Dispatch
                |- App.update App.Dispatch
                |- App.update (App.Finish 15)
                |== View.Finished
                        { duration = 10
                        , passed = 1
                        , outcome = Outcome.Pass
                        }
    , test "increments test counter" <|
        \_ ->
            init twoTests
                |- App.update (App.Start 5)
                |- App.update App.Dispatch
                |== View.Running
                        { completed = 1
                        , remaining = 1
                        , outcome = Outcome.Pass
                        }
    , test "captures failures" <|
        \_ ->
            init twoTests
                |- App.update (App.Start 5)
                |- App.update App.Dispatch
                |- App.update App.Dispatch
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
            init todoWithFailingTest
                |- App.update (App.Start 5)
                |- App.update App.Dispatch
                |- App.update App.Dispatch
                |== View.Running
                        { completed = 2
                        , remaining = 0
                        , outcome =
                            Outcome.Fail
                                [ ( [ "done", "todo then passing" ]
                                  , [ { given = Nothing, message = "just cause" } ]
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


{-| Extract first tuple element then map. Useful for piping update functions!
-}
(|-) : ( a, b ) -> (a -> c) -> c
(|-) ( a, _ ) f =
    f a
infixl 0 |-


{-| Shortcut for Expect.equal.
-}
(|==) : ( App.Model, b ) -> View.Model -> Expect.Expectation
(|==) ( appModel, _ ) viewModel =
    Expect.equal viewModel (App.present appModel)
infixl 0 |==
