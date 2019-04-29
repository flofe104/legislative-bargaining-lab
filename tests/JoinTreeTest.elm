module JoinTreeTest exposing (testIDs, testRefs)

import BargainLab exposing (..)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import JoinTreeTestHelper exposing (mergeCorrectIdGame, mergeToRefGame)
import QOBDD exposing (BDD(..), NodeId, QOBDD)
import QOBDDBuilders exposing (LookUpTables, NInfo, build, buildQOBDD, joinTree)
import SimpleGame exposing (..)
import Test exposing (..)


testIDs : Test
testIDs =
    describe "Testing IDs on JoinTree"
        [ test "Id Test on (Ref, Node) to Node merge" <|
            \_ ->
                Expect.equal
                    (buildQOBDD mergeCorrectIdGame)
                    (Just correctQobddIdTest)
        ]


testRefs : Test
testRefs =
    describe "Testing Refs on JoinTree"
        [ test "Ref Test on (Node, Ref) to Ref merge" <|
            \_ ->
                Expect.equal
                    (buildQOBDD mergeToRefGame)
                    (Just correctQobddRefNodeToRefTest)
        ]


correctQobddIdTest : QOBDD
correctQobddIdTest =
    QOBDD 2
        (Node
            { id = 2
            , elseB = Node { id = 1, thenB = Zero, var = 1, elseB = Zero }
            , var = 0
            , thenB = Node { id = 0, thenB = One, var = 1, elseB = One }
            }
        )


correctQobddRefNodeToRefTest : QOBDD
correctQobddRefNodeToRefTest =
    QOBDD 2
        (Node
            { id = 1
            , elseB = Ref { id = 0, bdd = correctRefNodeToRefBDD }
            , var = 0
            , thenB = correctRefNodeToRefBDD
            }
        )


correctRefNodeToRefBDD : BDD
correctRefNodeToRefBDD =
    Node { id = 0, thenB = One, var = 1, elseB = One }


inf : Float
inf =
    1 / 0


ninf : Float
ninf =
    -1 / 0
