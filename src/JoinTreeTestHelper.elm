module JoinTreeTestHelper exposing (displayMerge, mergeCorrectIdGame, mergeToRefGame, simpleGame, wrongPart1, wrongPart2)

import BargainLab exposing (..)
import Dict exposing (Dict)
import QOBDD exposing (BDD(..), NodeId)
import QOBDDBuilders exposing (ApplyData(..), ApplyState(..), NodeLookUpTable, apply, apply_, joinTree)
import SimpleGame exposing (..)


displayMerge : SimpleGame -> Maybe ( BDD, ApplyState )
displayMerge game =
    readMergeBDDs game.joinTree game.players game.rules


readMergeBDDs : JoinTree -> List Player -> List RuleMVG -> Maybe ( BDD, ApplyState )
readMergeBDDs jTree players rules =
    case jTree of
        Var str ->
            Nothing

        BinOp op tree1 tree2 ->
            case ( joinTree tree1 players rules, joinTree tree2 players rules ) of
                ( Just left, Just right ) ->
                    Just (apply_ (Data left op right) (ApplyState { nDict = Dict.empty, aDict = Dict.empty, id = 0 }))

                _ ->
                    Nothing


wrongPart1 : SimpleGame
wrongPart1 =
    SimpleGame 2 [ RuleMVG 1 [ 1, 0 ] ] 1 [ Player "a" 0, Player "b" 1 ] (Var "1")


wrongPart2 : SimpleGame
wrongPart2 =
    SimpleGame 2 [ RuleMVG 0 [ 0, 1 ] ] 1 [ Player "a" 0, Player "b" 1 ] (Var "1")


mergeToRefGame : SimpleGame
mergeToRefGame =
    SimpleGame 2 [ RuleMVG 1 [ 1, 0 ], RuleMVG 0 [ 0, 1 ] ] 2 [ Player "a" 0, Player "b" 1 ] (BinOp Or (Var "1") (Var "2"))


mergeCorrectIdGame : SimpleGame
mergeCorrectIdGame =
    SimpleGame 2 [ RuleMVG 1 [ 0, 0 ], RuleMVG 1 [ 1, 0 ] ] 2 [ Player "a" 0, Player "b" 1 ] (BinOp Or (Var "1") (Var "2"))


simpleGame : SimpleGame
simpleGame =
    SimpleGame 2 [ RuleMVG 1 [ 1, 1 ] ] 2 [ Player "a" 0, Player "b" 1 ] (Var "1")
