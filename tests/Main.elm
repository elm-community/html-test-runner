module Main exposing (main)

import Dict
import Set
import Expect
import Test exposing (..)
import Test.Runner.Html
import Test.Runner.Html.App as App
import Test.Runner.Html.View as View


-- FIXTURES


noTests : Test
noTests =
    describe "nothing" []


twoTests : Test
twoTests =
    describe "both"
        [ test "one" (\_ -> Expect.pass)
        , test "two" (\_ -> Expect.fail "message")
        ]



-- REAL TESTS


suite : List Test
suite =
    [ test "shows nothing on init" <|
        \_ ->
            App.init Nothing Nothing noTests
                |- App.present
                |== View.NotStarted
    , test "no tests has one (failure) expectation" <|
        \_ ->
            App.init Nothing Nothing noTests
                |- App.update (App.Start 5)
                |- App.present
                |== View.Running
                        { completed = 0
                        , remaining = 1
                        , failures = []
                        }
    , test "fails when describe has no tests" <|
        \_ ->
            App.init Nothing Nothing noTests
                |- App.update (App.Start 5)
                |- App.update App.Dispatch
                |- App.update (App.Finish 10)
                |- App.present
                |== View.Finished
                        { duration = 5
                        , passed = 0
                        , failures =
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
    , test "increments test counter" <|
        \_ ->
            App.init Nothing Nothing twoTests
                |- App.update (App.Start 5)
                |- App.update App.Dispatch
                |- App.present
                |== View.Running
                        { completed = 1
                        , remaining = 1
                        , failures = []
                        }
    , test "captures failures" <|
        \_ ->
            App.init Nothing Nothing twoTests
                |- App.update (App.Start 5)
                |- App.update App.Dispatch
                |- App.update App.Dispatch
                |- App.present
                |== View.Running
                        { completed = 2
                        , remaining = 0
                        , failures =
                            [ ( [ "two", "both" ]
                              , [ { given = Nothing, message = "message" } ]
                              )
                            ]
                        }
    ]


main : Test.Runner.Html.TestProgram
main =
    describe "Test.Runner.Html.App" suite
        |> Test.Runner.Html.run


{-| Extract first tuple element then map. Useful for piping update functions!
-}
(|-) : ( a, b ) -> (a -> c) -> c
(|-) ( a, _ ) f =
    f a
infixl 0 |-


{-| Shortcut for Expect.equal.
-}
(|==) : a -> a -> Expect.Expectation
(|==) top bottom =
    Expect.equal bottom top
infixl 0 |==
