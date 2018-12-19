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


{-| A `SimpleGraph` is one in which
the nodes have strings as labels
and the edges have floats as labels.
The edge label represents the flow
of some quantity from node to node.

Note that a SimpleGraph is an instance
of the `Graph n e` type.

-}
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


{-| Compute the sum of the labels on the incoming edges of
the node with the given id.
-}
inFlow : Int -> SimpleGraph -> Float
inFlow nodeId graph =
    Graph.get nodeId graph
        |> Maybe.map .incoming
        |> Maybe.map IntDict.values
        |> Maybe.withDefault []
        |> List.sum


{-| Compute the sum of the labels on the edges of the graph.
-}
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
