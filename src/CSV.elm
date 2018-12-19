module CSV exposing (graphFromString, stringFromGraph)

import Graph
import SimpleGraph exposing (SimpleGraph, Node, Edge)
import Parser exposing (..)
import Parser.Extras exposing (many)
import List.Extra exposing (unique)
import Maybe.Extra
import Dict exposing (Dict)


type SimpleEdge
    = SimpleEdge String String Float


{-| Build a graph from a string representing the graph in CSV format.

> data = "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n"
> "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n" : String

> graphFromString data
> Graph (Inner { left = Inner { left = Leaf { key = 0, value = { incoming = Empty, node = { id = 0, label = "A" }, outgoing = Inner { left = Leaf { key = 1, value = 1 }, prefix = { branchingBit = 2, prefixBits = 0 }, right = Leaf { key = 2, value = 2 }, size = 2 } } }, prefix = { branchingBit = 1, prefixBits = 0 }, right = Leaf { key = 1, value = { incoming = Leaf { key = 0, value = 1 }, node = { id = 1, label = "B" }, outgoing = Leaf { key = 3, value = 3 } } }, size = 2 }, prefix = { branchingBit = 2, prefixBits = 0 }, right = Inner { left = Leaf { key = 2, value = { incoming = Leaf { key = 0, value = 2 }, node = { id = 2, label = "C" }, outgoing = Leaf { key = 3, value = 4 } } }, prefix = { branchingBit = 1, prefixBits = 2 }, right = Leaf { key = 3, value = { incoming = Inner { left = Leaf { key = 1, value = 3 }, prefix = { branchingBit = 2, prefixBits = 0 }, right = Leaf { key = 2, value = 4 }, size = 2 }, node = { id = 3, label = "D" }, outgoing = Empty } }, size = 2 }, size = 4 })

There is also an inverse operation:

> data |> graphFromString |> stringFromGraph
> "A,B,1\nA,C,2\nB,D,3\nC,D,4"

-}
graphFromString : String -> SimpleGraph
graphFromString csvString =
    let
        simpleEdgeList =
            simpleEdgeListFromString csvString

        dict =
            simpleEdgeList |> nodeNameList |> nodeDictFromNames

        nodes =
            nodesFromNodeDict dict

        edges =
            edgeListUsingDict dict simpleEdgeList
    in
        Graph.fromNodesAndEdges nodes edges


{-|

> data |> graphFromString |> stringFromGraph
> "A,B,1\nA,C,2\nB,D,3\nC,D,4"
-}
stringFromGraph : SimpleGraph -> String
stringFromGraph graph =
    graph
        |> simpleEdgeListFromGraph
        |> List.map stringFromSimpleEdge
        |> List.reverse
        |> String.join ("\n")


stringFromSimpleEdge : SimpleEdge -> String
stringFromSimpleEdge (SimpleEdge from to label) =
    from ++ "," ++ to ++ "," ++ String.fromFloat label



-- export : SimpleGraph -> String
-- export graph =


{-|

> data
> "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n"

> g = build data
> Graph ..

> simpleEdgeListFromGraph g
> [SimpleEdge "C" "D" 4,SimpleEdge "B" "D" 3,SimpleEdge "A" "C" 2,SimpleEdge "A" "B" 1]

-}
simpleEdgeListFromGraph : SimpleGraph -> List SimpleEdge
simpleEdgeListFromGraph graph =
    let
        dict =
            graph |> Graph.nodes |> dictFromNodeList

        edges =
            graph |> Graph.edges
    in
        simpleEdgeListFromEdgeList dict edges


addNodeToDict : Node -> Dict Int String -> Dict Int String
addNodeToDict node dict =
    Dict.insert node.id node.label dict


pairFromNode : Node -> ( Int, String )
pairFromNode node =
    ( node.id, node.label )


dictFromNodeList : List Node -> Dict Int String
dictFromNodeList nodeList =
    nodeList
        |> List.map pairFromNode
        |> Dict.fromList


simpleEdgeListFromEdgeList : Dict Int String -> List Edge -> List SimpleEdge
simpleEdgeListFromEdgeList dict edgeList =
    edgeList
        |> List.map (simpleEdgeFromEdge dict)
        |> Maybe.Extra.values


simpleEdgeFromEdge : Dict Int String -> Edge -> Maybe SimpleEdge
simpleEdgeFromEdge dict edge =
    let
        sourceNode_ =
            Dict.get edge.from dict

        targetNode_ =
            Dict.get edge.to dict
    in
        Maybe.map3 SimpleEdge sourceNode_ targetNode_ (Just edge.label)


{-|

> data = "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n"
> "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n" : String

> dict = data |> simpleEdgeListFromString |> nodeNameList |> nodeDictFromNames
> Dict.fromList [("A",0),("B",1),("C",2),("D",3)]

-}
edgeListUsingDict : Dict String Int -> List SimpleEdge -> List Edge
edgeListUsingDict dict simpleEdgeList =
    let
        getId key =
            Dict.get key dict |> Maybe.withDefault -1

        edgeFromSimpleEdge (SimpleEdge from to label) =
            { from = getId from, to = getId to, label = label }
    in
        simpleEdgeList |> List.map edgeFromSimpleEdge


{-|

> ["A", "B"] |> nodeDictFromNames |> nodesFromNodeDict
> [{ id = 0, label = "A" },{ id = 1, label = "B" }]
-}
nodesFromNodeDict : Dict String Int -> List Node
nodesFromNodeDict dict =
    let
        getId key =
            Dict.get key dict |> Maybe.withDefault -1
    in
        Dict.keys dict
            |> List.map (\key -> { id = getId key, label = key })


{-|

> nodeDictFromNames ["A", "B"]
> Dict.fromList [("A",0),("B",1)]
-}
nodeDictFromNames : List String -> Dict String Int
nodeDictFromNames nodeNames =
    nodeNames
        |> List.indexedMap Tuple.pair
        |> List.map swap
        |> Dict.fromList


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )



{- vvv SimpleEdge vvv -}


sourceNode : SimpleEdge -> String
sourceNode (SimpleEdge source target label) =
    source


targetNode : SimpleEdge -> String
targetNode (SimpleEdge source target label) =
    target


{-| Compute a list of node labels from a list of
simple edges.

> data = "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n"
> "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n" : String

> data |> simpleEdgeListFromString |> nodeNameList
> ["A","B","C","D"] : List String

-}
nodeNameList : List SimpleEdge -> List String
nodeNameList edgeList =
    (List.map sourceNode edgeList ++ List.map targetNode edgeList)
        |> unique
        |> List.sort



{- vvv Parsing code vvv -}


{-|

> data = "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n"
> "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n" : String
> data |> simpleEdgeListFromString
> [SimpleEdge "A" "B" 1,SimpleEdge "A" "C" 2,SimpleEdge "B" "D" 3,SimpleEdge "C" "D" 4]
-}
simpleEdgeListFromString : String -> List SimpleEdge
simpleEdgeListFromString str =
    case run simpleEdgeListParser (String.trim str) of
        Err _ ->
            []

        Ok edgeList ->
            edgeList


simpleEdgeListParser : Parser (List SimpleEdge)
simpleEdgeListParser =
    many simpleEdgeParser


simpleEdgeParser : Parser SimpleEdge
simpleEdgeParser =
    succeed SimpleEdge
        |= identifier
        |. symbol ","
        |. spaces
        |= identifier
        |. symbol ","
        |. spaces
        |= float
        |. spaces


identifier : Parser String
identifier =
    getChompedString <|
        succeed ()
            |. chompIf isStartChar
            |. chompWhile isInnerChar


endsWithChar : Char -> Parser String
endsWithChar endChar =
    getChompedString <|
        succeed ()
            |. chompWhile (\char -> char /= endChar)


isStartChar : Char -> Bool
isStartChar char =
    Char.isAlpha char


isInnerChar : Char -> Bool
isInnerChar char =
    isStartChar char || Char.isDigit char


normalize : String -> String
normalize str =
    str
        |> String.trim
        |> \x -> x ++ "\n"
