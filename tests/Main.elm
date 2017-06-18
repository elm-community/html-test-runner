module Main exposing (..)

import Expect
import Fixtures
import Test exposing (..)
import Test.Runner.Exploration as Runner
import Test.Runner.Html
import Test.Runner.Html.App as App
import Test.Runner.Html.View as View


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
                        , Runner.Fail 0
                            [ ( []
                              , [ { given = Nothing
                                  , message = .noTests Fixtures.description
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
                |== Just ( 10, Runner.Pass 1 )
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
                        , Runner.Fail 0
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
                        , Runner.Todo 1
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
                |== Just ( 94, Runner.AutoFail 1 Runner.Only )
    , test "shows skip in isolation" <|
        \_ ->
            init (Fixtures.noTests >> skip)
                |- App.update (App.Dispatch 5)
                |- App.update (App.Dispatch 99)
                |== Just ( 94, Runner.AutoFail 0 Runner.Skip )
    , test "invalid test shows custom reason" <|
        \_ ->
            App.init 0 Nothing (describe "asdf" [])
                |- App.update (App.Dispatch 5)
                |== Just
                        ( 0
                        , Fixtures.description
                            |> .invalid
                            |> Runner.Custom
                            |> Runner.AutoFail 0
                        )
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
    { passed : Int, remaining : Int, failures : List Runner.Failure }
    -> App.Model
    -> Expect.Expectation
expectRunning expected model =
    case App.present model of
        Just ( _, Runner.Running { passed, remaining, failures } ) ->
            Expect.equal
                ( expected.passed, expected.remaining, expected.failures )
                ( passed, remaining, failures )

        _ ->
            "Expected Runner.Running "
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
