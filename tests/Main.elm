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



-- REAL TESTS


suite : Test
suite =
    describe "Test.Runner.Html.App"
        [ test "shows nothing on init" <|
            \_ ->
                App.init Nothing Nothing noTests
                    |- App.present
                    |> Expect.equal View.NotStarted
        , test "shows running on Start" <|
            \_ ->
                App.init Nothing Nothing noTests
                    |- App.update (App.Start 5)
                    |- App.present
                    |> Expect.equal
                        (View.Running
                            { completed = 0
                            , remaining = 0
                            , failures = []
                            }
                        )
        , test "finishes when none remaining Finish" <|
            \_ ->
                App.init Nothing Nothing noTests
                    |- App.update (App.Start 5)
                    |- App.update (App.Finish 10)
                    |- App.present
                    |> Expect.equal
                        (View.Finished
                            { duration = 5
                            , passed = 0
                            , failures = []
                            }
                        )
        ]


main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run suite


{-| Extract first tuple element then map. Useful for piping update functions!
-}
(|-) : ( a, b ) -> (a -> c) -> c
(|-) ( a, _ ) f =
    f a
infixl 0 |-
