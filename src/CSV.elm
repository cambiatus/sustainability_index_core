module CSV exposing (build, nodeNameList, simpleEdgeListFromString, edgeListUsingDict, nodesFromNodeDict, nodeDictFromNames)

import Graph
import SimpleGraph exposing (SimpleGraph, Node, Edge)
import Parser exposing (..)
import Parser.Extras exposing (many)
import List.Extra exposing (unique)
import Dict exposing (Dict)


{-| Build a graph from a string representing the graph in CSV format.

> data = "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n"
> "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n" : String

> build data
> Graph (Inner { left = Inner { left = Leaf { key = 0, value = { incoming = Empty, node = { id = 0, label = "A" }, outgoing = Inner { left = Leaf { key = 1, value = 1 }, prefix = { branchingBit = 2, prefixBits = 0 }, right = Leaf { key = 2, value = 2 }, size = 2 } } }, prefix = { branchingBit = 1, prefixBits = 0 }, right = Leaf { key = 1, value = { incoming = Leaf { key = 0, value = 1 }, node = { id = 1, label = "B" }, outgoing = Leaf { key = 3, value = 3 } } }, size = 2 }, prefix = { branchingBit = 2, prefixBits = 0 }, right = Inner { left = Leaf { key = 2, value = { incoming = Leaf { key = 0, value = 2 }, node = { id = 2, label = "C" }, outgoing = Leaf { key = 3, value = 4 } } }, prefix = { branchingBit = 1, prefixBits = 2 }, right = Leaf { key = 3, value = { incoming = Inner { left = Leaf { key = 1, value = 3 }, prefix = { branchingBit = 2, prefixBits = 0 }, right = Leaf { key = 2, value = 4 }, size = 2 }, node = { id = 3, label = "D" }, outgoing = Empty } }, size = 2 }, size = 4 })

-}
build : String -> SimpleGraph
build csvString =
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


type SimpleEdge
    = SimpleEdge String String Float


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
