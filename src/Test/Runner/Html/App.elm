module Test.Runner.Html.App exposing (Model, Msg, run)

{-| Test runner for a Html.App

@docs run, Model, Msg

-}

import Test exposing (Test)
import Test.Runner exposing (Runner(..))
import Expect exposing (Expectation)
import Html exposing (Html, text)
import Task
import Random.Pcg as Random
import Time exposing (Time)


{-| -}
type Msg subMsg
    = Init Time
    | SubMsg subMsg


{-| -}
type Model subMsg subModel
    = Uninitialized (SubUpdate subMsg subModel) (Maybe Random.Seed) Int Test (Time -> List (() -> ( List String, List Expectation )) -> ( subModel, Cmd subMsg ))
    | Initialized (SubUpdate subMsg subModel) subModel


timeToSeed : Time -> Random.Seed
timeToSeed time =
    (0xFFFFFFFF * time)
        |> floor
        |> Random.initialSeed


initOrUpdate : Msg subMsg -> Model subMsg subModel -> ( Model subMsg subModel, Cmd (Msg subMsg) )
initOrUpdate msg maybeModel =
    case maybeModel of
        Uninitialized update seed runs test init ->
            case msg of
                Init time ->
                    let
                        finalSeed =
                            case seed of
                                Just realSeed ->
                                    realSeed

                                Nothing ->
                                    timeToSeed time

                        ( subModel, subCmd ) =
                            test
                                |> Test.Runner.fromTest runs finalSeed
                                |> toThunks
                                |> init time
                    in
                        ( Initialized update subModel, Cmd.map SubMsg subCmd )

                SubMsg _ ->
                    Debug.crash "Attempted to run a SubMsg pre-Init!"

        Initialized update model ->
            case msg of
                SubMsg subMsg ->
                    let
                        ( newModel, cmd ) =
                            update subMsg model
                    in
                        ( Initialized update newModel, Cmd.map SubMsg cmd )

                Init _ ->
                    Debug.crash "Attempted to init twice!"


initOrView : (subModel -> Html subMsg) -> Model subMsg subModel -> Html (Msg subMsg)
initOrView view model =
    case model of
        Uninitialized _ _ _ _ _ ->
            text ""

        Initialized _ subModel ->
            Html.map SubMsg (view subModel)


type alias SubUpdate msg model =
    msg -> model -> ( model, Cmd msg )


type alias RunnerOptions =
    { seed : Maybe Random.Seed
    , runs : Maybe Int
    }


type alias AppOptions msg model =
    { init : Time -> List (() -> ( List String, List Expectation )) -> ( model, Cmd msg )
    , update : SubUpdate msg model
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }


subscriptions : (subModel -> Sub subMsg) -> Model subMsg subModel -> Sub (Msg subMsg)
subscriptions subs model =
    case model of
        Uninitialized _ _ _ _ _ ->
            Sub.none

        Initialized _ subModel ->
            Sub.map SubMsg (subs subModel)


toThunks : Runner -> List (() -> ( List String, List Expectation ))
toThunks =
    toThunksHelp []


toThunksHelp : List String -> Runner -> List (() -> ( List String, List Expectation ))
toThunksHelp labels runner =
    case runner of
        Runnable runnable ->
            [ \() -> ( labels, Test.Runner.run runnable ) ]

        Labeled label subRunner ->
            toThunksHelp (label :: labels) subRunner

        Batch runners ->
            List.concatMap (toThunksHelp labels) runners


{-| Run the tests and render the results as a Web page.
-}
run : RunnerOptions -> AppOptions msg model -> Test -> Program Never (Model msg model) (Msg msg)
run runnerOpts appOpts test =
    let
        runs =
            Maybe.withDefault defaultRunCount runnerOpts.runs

        cmd =
            Task.perform Init Time.now

        init =
            ( Uninitialized appOpts.update runnerOpts.seed runs test appOpts.init, cmd )
    in
        Html.program
            { init = init
            , update = initOrUpdate
            , view = initOrView appOpts.view
            , subscriptions = subscriptions appOpts.subscriptions
            }


defaultRunCount : Int
defaultRunCount =
    100
