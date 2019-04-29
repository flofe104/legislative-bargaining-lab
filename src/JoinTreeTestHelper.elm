module JoinTreeTestHelper exposing (displayMerge, failingGame, simpleGame, wrongPart1, wrongPart2)

import BargainLab exposing (..)
import Dict exposing (Dict)
import QOBDD exposing (BDD(..), NodeId)
import QOBDDBuilders exposing (apply, joinTree)
import SimpleGame exposing (..)


displayMerge : SimpleGame -> Maybe ( BDD, Dict ( NodeId, NodeId, Int ) BDD )
displayMerge game =
    readMergeBDDs game.joinTree game.players game.rules


readMergeBDDs : JoinTree -> List Player -> List RuleMVG -> Maybe ( BDD, Dict ( NodeId, NodeId, Int ) BDD )
readMergeBDDs jTree players rules =
    case jTree of
        Var str ->
            Nothing

        BinOp op tree1 tree2 ->
            case ( joinTree tree1 players rules, joinTree tree2 players rules ) of
                ( Just left, Just right ) ->
                    Just (apply left right op Dict.empty)

                _ ->
                    Nothing


wrongPart1 : SimpleGame
wrongPart1 =
    SimpleGame 2 [ RuleMVG 1 [ 1, 0 ] ] 1 [ Player "a" 0, Player "b" 1 ] (Var "1")


wrongPart2 : SimpleGame
wrongPart2 =
    SimpleGame 2 [ RuleMVG 0 [ 0, 1 ] ] 1 [ Player "a" 0, Player "b" 1 ] (Var "1")


failingGame : SimpleGame
failingGame =
    SimpleGame 2 [ RuleMVG 1 [ 1, 0 ], RuleMVG 0 [ 0, 1 ] ] 2 [ Player "a" 0, Player "b" 1 ] (BinOp Or (Var "1") (Var "2"))


simpleGame : SimpleGame
simpleGame =
    SimpleGame 2 [ RuleMVG 1 [ 1, 1 ] ] 2 [ Player "a" 0, Player "b" 1 ] (Var "1")
