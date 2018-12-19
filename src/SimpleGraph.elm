module SimpleGraph
    exposing
        ( SimpleGraph
        , Node
        , Edge
        , outFlow
        , inFlow
        , totalFlow
        , nodeCount
        , edgeCount
        )

import Graph exposing (Graph)
import IntDict
import Maybe.Extra


type alias SimpleGraph =
    Graph String Float


type alias Node =
    Graph.Node String


type alias Edge =
    Graph.Edge Float


{-| Compute the sum of the labels on the outgoing edges of
the node with the given id.
-}
outFlow : Int -> SimpleGraph -> Float
outFlow nodeId graph =
    Graph.get nodeId graph
        |> Maybe.map .outgoing
        |> Maybe.map IntDict.values
        |> Maybe.withDefault []
        |> List.sum


inFlow : Int -> SimpleGraph -> Float
inFlow nodeId graph =
    Graph.get nodeId graph
        |> Maybe.map .incoming
        |> Maybe.map IntDict.values
        |> Maybe.withDefault []
        |> List.sum


totalFlow : SimpleGraph -> Float
totalFlow graph =
    graph
        |> Graph.edges
        |> List.map .label
        |> List.sum


nodeCount : SimpleGraph -> Int
nodeCount graph =
    graph
        |> Graph.nodes
        |> List.length


edgeCount : SimpleGraph -> Int
edgeCount graph =
    graph
        |> Graph.edges
        |> List.length



--
-- testGraph1 : SimpleGraph
-- testGraph1 =
--     Graph.fromNodesAndEdges
--         [ { id = 1, label = "1" }, { id = 2, label = "2" } ]
--         [ { from = 1, to = 2, label = 1.3 } ]
--
--
-- testGraph2 : SimpleGraph
-- testGraph2 =
--     Graph.fromNodesAndEdges
--         [ { id = 1, label = "1" }
--         , { id = 2, label = "2" }
--         , { id = 3, label = "3" }
--         , { id = 4, label = "4" }
--         ]
--         [ { from = 1, to = 2, label = 1.0 }
--         , { from = 2, to = 3, label = 1.0 }
--         , { from = 2, to = 4, label = 1.0 }
--         ]
--
--
-- testGraph3 : SimpleGraph
-- testGraph3 =
--     Graph.fromNodesAndEdges
--         [ { id = 1, label = "1" }
--         , { id = 2, label = "2" }
--         , { id = 3, label = "3" }
--         , { id = 4, label = "4" }
--         , { id = 5, label = "5" }
--         ]
--         [ { from = 1, to = 2, label = 1.0 }
--         , { from = 2, to = 3, label = 1.0 }
--         , { from = 2, to = 4, label = 1.0 }
--         , { from = 2, to = 5, label = 1.0 }
--         ]
