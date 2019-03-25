module Example exposing (empty, simple)

import BargainLab exposing (..)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import QOBDD exposing (BDD(..), NodeId)
import QOBDDBuilders exposing (LookUpTables, NInfo, buildRec)
import SimpleGame exposing (..)
import Test exposing (..)


empty : Test
empty =
    describe "empty game"
        [ describe "empty"
            [ test "losing" <|
                \_ -> Expect.equal (buildRec 0 1 [] [] Dict.empty) ( 0, NInfo Zero 0 (1 / 0), Dict.empty )
            , test "winning" <|
                \_ -> Expect.equal (buildRec 0 0 [] [] Dict.empty) ( 0, NInfo One (-1 / 0) 0, Dict.empty )
            ]
        ]


simple : Test
simple =
    describe "simpleGame"
        [ describe "OnePlayer"
            [ test "Losing" <|
                \_ -> Expect.equal (buildRec 0 2 [ 1 ] [ Player "a" 0 ] Dict.empty) ( 1, { v = Node { elseB = Zero, id = 0, thenB = Zero, var = 0 }, x = 1, y = inf }, onePlayerLosingDictionary )
            , test "Depending" <|
                \_ -> Expect.equal (buildRec 0 1 [ 1 ] [ Player "a" 0 ] Dict.empty) ( 1, { v = Node { elseB = Zero, id = 0, thenB = One, var = 0 }, x = 0, y = 1 }, onePlayerPendingDictionary )
            , test "Winning" <|
                \_ -> Expect.equal (buildRec 0 0 [ 1 ] [ Player "a" 0 ] Dict.empty) ( 1, { v = Node { elseB = One, id = 0, thenB = One, var = 0 }, x = ninf, y = 0 }, onePlayerWinningDictionary )
            ]

        {- , describe "TwoPlayer"
           [ test "All Losing" <|
               \_ -> Expect.equal (buildRec 0 3 [ 1, 1 ] [ Player "a" 0, Player "b" 1 ] Dict.empty) ( 2, NInfo Zero 0 (1 / 0), onePlayerLosingDictionary )
           ]
        -}
        ]


onePlayerLosingDictionary : LookUpTables
onePlayerLosingDictionary =
    Dict.fromList [ ( 0, [ { v = Ref { bdd = Node { elseB = Zero, id = 0, thenB = Zero, var = 0 }, id = 0 }, x = 1, y = inf } ] ) ]


onePlayerPendingDictionary : LookUpTables
onePlayerPendingDictionary =
    Dict.fromList [ ( 0, [ { v = Ref { bdd = Node { elseB = Zero, id = 0, thenB = One, var = 0 }, id = 0 }, x = 0, y = 1 } ] ) ]


onePlayerWinningDictionary : LookUpTables
onePlayerWinningDictionary =
    Dict.fromList [ ( 0, [ { v = Ref { bdd = Node { elseB = One, id = 0, thenB = One, var = 0 }, id = 0 }, x = ninf, y = 0 } ] ) ]


inf : Float
inf =
    1 / 0


ninf : Float
ninf =
    -1 / 0
