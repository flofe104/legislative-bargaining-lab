module QOBDDBuilders exposing
    ( ApplyState(..)
    , BuildRecState(..)
    , LookUpTables
    , NInfo
    , NodeLookUpTable
    , apply
    , apply_
    , build
    , buildQOBDD
    , buildRec
    , buildRec_
    , insert
    , joinTree
    , lookup
    )

import Dict exposing (Dict)
import QOBDD exposing (..)
import SimpleGame exposing (..)
import State exposing (State)


{-| x is the smallest weight in the winning coalition of a node. If all coalitions are winning is
x = - inf. Are all coalitions loosing is x = 0.
y is the largest weight in the loosing coalition of a node. If all coalitions are losing is y = inf
are all coalitions winning is x = 0.
-}
type alias NInfo =
    { v : BDD, x : Float, y : Float }


{-| Contains the lookup table for each player. (The single lookup tables should be AVL Trees)
-}
type alias LookUpTables =
    Dict PlayerId (List NInfo)


{-| The function tries to find a sub-tree for player i that has already
been created and can be used for the given quota again. (for the buildRec algorithm)
-}
lookup : LookUpTables -> PlayerId -> Quota -> Maybe NInfo
lookup tables playerId quota =
    case Dict.get playerId tables of
        Nothing ->
            Nothing

        Just table ->
            case List.filter (\info -> (info.x < toFloat quota) && (info.y >= toFloat quota)) table of
                [] ->
                    Nothing

                [ info ] ->
                    Just info

                infos ->
                    Nothing


{-| Insert a sub-tree in the LookUpTable for a specific player. (should be implemented as AVL Tree)
-}
insert2 : LookUpTables -> PlayerId -> NInfo -> LookUpTables
insert2 tables playerId nodeInfo =
    case Dict.get playerId tables of
        Nothing ->
            Dict.insert playerId [ nodeInfo ] tables

        Just table ->
            Dict.insert playerId (nodeInfo :: table) tables


type NodeState
    = NodeState { nDict : NodeLookUpTable, id : Int }


ite : Int -> BDD -> BDD -> State NodeState BDD
ite i t e =
    State.andThen State.get
        --`andThen`
        (\(NodeState s) ->
            case Dict.get ( bddId t, i, bddId e ) s.nDict of
                Nothing ->
                    let
                        newNode =
                            Node { id = s.id, thenB = t, var = i, elseB = e }

                        ref =
                            Ref { id = s.id, bdd = newNode }
                    in
                    State.andThen
                        (State.put
                            (NodeState
                                { nDict = Dict.insert ( bddId t, i, bddId e ) ref s.nDict
                                , id = s.id + 1
                                }
                            )
                        )
                        --`andThen`
                        (\_ -> State.return newNode)

                Just ref ->
                    State.return ref
        )


type BuildRecState
    = BuildRecState { table : LookUpTables, id : Int }


buildRec :
    Quota
    -> List PlayerWeight
    -> List Player
    -> NInfo
buildRec quota weights players =
    Tuple.first <| buildRec_ quota weights players (BuildRecState { table = Dict.empty, id = 0 })


{-| The function is used to build a single BDD.
-}
buildRec_ :
    Quota
    -> List PlayerWeight
    -> List Player
    -> State BuildRecState NInfo
buildRec_ quota weights players =
    case ( weights, players ) of
        ( w :: ws, p :: ps ) ->
            State.andThen State.get
                --`andThen`
                (\(BuildRecState s) ->
                    case lookup s.table p.id quota of
                        Just nodeInfo ->
                            State.return nodeInfo

                        Nothing ->
                            State.andThen (buildRec_ (quota - w) ws ps)
                                --`andThen`
                                (\infoT ->
                                    State.andThen (buildRec_ quota ws ps)
                                        --`andThen`
                                        (\infoE ->
                                            State.andThen State.get
                                                (\(BuildRecState s2) ->
                                                    let
                                                        {- ( nodeId2, infoT, tables2 ) =
                                                               buildRec nodeId1 (quota - w) ws ps tables1

                                                           ( nodeId3, infoE, tables3 ) =
                                                               buildRec nodeId2 quota ws ps tables2
                                                        -}
                                                        ( x1, y1 ) =
                                                            ( max (infoT.x + toFloat w) infoE.x, min (infoT.y + toFloat w) infoE.y )

                                                        node =
                                                            Node { id = s2.id, thenB = infoT.v, var = p.id, elseB = infoE.v }

                                                        info =
                                                            { v = node, x = x1, y = y1 }

                                                        ref =
                                                            Ref { id = s2.id, bdd = node }

                                                        refInfo =
                                                            { v = ref, x = x1, y = y1 }
                                                    in
                                                    State.andThen
                                                        (State.put
                                                            (BuildRecState
                                                                { table = insert2 s2.table p.id refInfo
                                                                , id = s2.id + 1
                                                                }
                                                            )
                                                        )
                                                        --`andThen`
                                                        (\_ -> State.return info)
                                                )
                                        )
                                )
                )

        ( _, _ ) ->
            if quota > 0 then
                State.return { v = Zero, x = 0, y = 1 / 0 }

            else
                State.return { v = One, x = -1 / 0, y = 0 }


op2Int : Op -> Int
op2Int op =
    case op of
        And ->
            0

        Or ->
            1


{-| Abstract used to circumvent restriction that keys of a dictionary have to be comparable
-}
get : ( NodeId, NodeId, Op ) -> Dict ( NodeId, NodeId, Int ) BDD -> Maybe BDD
get ( node1, node2, op ) =
    Dict.get ( node1, node2, op2Int op )


insert :
    ( NodeId, NodeId, Op )
    -> BDD
    -> Dict ( NodeId, NodeId, Int ) BDD
    -> Dict ( NodeId, NodeId, Int ) BDD
insert ( node1, node2, op ) bdd =
    Dict.insert ( node1, node2, op2Int op ) bdd


bddId : BDD -> Int
bddId bdd =
    case bdd of
        Zero ->
            -2

        One ->
            -1

        Node n ->
            n.id

        Ref r ->
            r.id


{-| Contains all Nodes that have been created during Apply
-}
type alias NodeLookUpTable =
    Dict ( NodeId, PlayerId, NodeId ) BDD


{-| Contains all Node pairs that have been applied
-}
type alias ApplyLookUpTable =
    Dict ( NodeId, NodeId, Int ) BDD


type ApplyState
    = ApplyState { nDict : NodeLookUpTable, aDict : ApplyLookUpTable, id : Int }


apply_ : BDD -> Op -> BDD -> State ApplyState BDD
apply_ tree1 op tree2 =
    let
        applyNonRefs a b =
            State.andThen (apply_ a.thenB op b.thenB)
                --`andThen`
                (\lBdd ->
                    State.andThen (apply_ a.elseB op b.elseB)
                        --`andThen`
                        (\rBdd ->
                            State.andThen State.get
                                --`andThen`
                                (\(ApplyState s) ->
                                    case Dict.get ( bddId lBdd, a.var, bddId rBdd ) s.nDict of
                                        Just ref ->
                                            State.return ref

                                        Nothing ->
                                            let
                                                newNode =
                                                    Node { id = s.id, thenB = lBdd, var = a.var, elseB = rBdd }

                                                ref =
                                                    Ref { id = s.id, bdd = newNode }
                                            in
                                            State.andThen
                                                (State.put
                                                    (ApplyState
                                                        { nDict = Dict.insert ( bddId lBdd, a.var, bddId rBdd ) ref s.nDict
                                                        , aDict = insert ( a.id, b.id, op ) ref s.aDict
                                                        , id = s.id + 1
                                                        }
                                                    )
                                                )
                                                --`andThen`
                                                (\_ -> State.return newNode)
                                )
                        )
                )
    in
    case ( tree1, tree2 ) of
        ( Zero, _ ) ->
            case op of
                And ->
                    State.return Zero

                Or ->
                    State.return tree2

        ( _, Zero ) ->
            case op of
                And ->
                    State.return Zero

                Or ->
                    State.return tree1

        ( One, _ ) ->
            case op of
                And ->
                    State.return tree2

                Or ->
                    State.return One

        ( _, One ) ->
            case op of
                And ->
                    State.return tree1

                Or ->
                    State.return One

        ( Ref a, Ref b ) ->
            State.andThen State.get
                --`andThen`
                (\(ApplyState s) ->
                    case Dict.get ( a.id, b.id, op2Int op ) s.aDict of
                        Just refNode ->
                            State.return refNode

                        Nothing ->
                            apply_ a.bdd op b.bdd
                )

        ( Ref a, b ) ->
            apply_ a.bdd op b

        ( a, Ref b ) ->
            apply_ a op b.bdd

        ( Node a, Node b ) ->
            applyNonRefs a b


{-| Creates a BDD by applying a binary operation to two BDD's.
-}
apply : BDD -> BDD -> Op -> Dict ( NodeId, NodeId, Int ) BDD -> ( BDD, Dict ( NodeId, NodeId, Int ) BDD )
apply tree1 tree2 op dict1 =
    case apply_ tree1 op tree2 (ApplyState { nDict = dict1, aDict = Dict.empty, id = 0 }) of
        ( bdd, ApplyState state ) ->
            ( bdd, state.nDict )


{-| Uses apply to create a single BDD from a JoinTree.
-}
joinTree : JoinTree -> List Player -> List RuleMVG -> Maybe BDD
joinTree jTree players rules =
    case jTree of
        Var str ->
            case String.toInt str of
                Just ruleid ->
                    case List.drop (ruleid - 1) rules of
                        r :: rs ->
                            Just (build r players)

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        BinOp op tree1 tree2 ->
            case ( joinTree tree1 players rules, joinTree tree2 players rules ) of
                ( Just left, Just right ) ->
                    Just (Tuple.first (apply left right op Dict.empty))

                _ ->
                    Nothing


{-| Uses the buildRec algorithm to create a BDD based on the rule defined in the RuleMVG type.
-}
build : RuleMVG -> List Player -> BDD
build rule players =
    (buildRec rule.quota rule.weights players).v


{-| Builds a QOBDD based on a single single rule or an entire JoinTree.
-}
buildQOBDD : SimpleGame -> Maybe QOBDD
buildQOBDD game =
    case joinTree game.joinTree game.players game.rules of
        Nothing ->
            Nothing

        Just bdd ->
            Just (QOBDD game.playerCount bdd)
