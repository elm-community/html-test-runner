module Main exposing (..)

import Test.Runner.Html
import TestRunnerExplorationTest


main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run TestRunnerExplorationTest.suite
