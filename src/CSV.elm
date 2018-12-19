module CSV exposing (build)

import SimpleGraph exposing (SimpleGraph, Node, Edge)
import Parser exposing (..)
import Parser.Extras exposing (many)
import List.Extra exposing (unique)
import Dict exposing (Dict)
import Graph


type SimpleEdge
    = SimpleEdge String String Float


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


edgeListUsingDict : Dict String Int -> List SimpleEdge -> List Edge
edgeListUsingDict dict simpleEdgeList =
    let
        getId key =
            Dict.get key dict |> Maybe.withDefault -1

        edgeFromSimpleEdge (SimpleEdge from to label) =
            { from = getId from, to = getId to, label = label }
    in
        simpleEdgeList |> List.map edgeFromSimpleEdge


nodesFromNodeDict : Dict String Int -> List Node
nodesFromNodeDict dict =
    let
        getId key =
            Dict.get key dict |> Maybe.withDefault -1
    in
        Dict.keys dict
            |> List.map (\key -> { id = getId key, label = key })


nodeDictFromNames : List String -> Dict String Int
nodeDictFromNames nodeNames =
    nodeNames
        |> List.indexedMap Tuple.pair
        |> List.map swap
        |> Dict.fromList


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


sourceNode : SimpleEdge -> String
sourceNode (SimpleEdge source target label) =
    source


targetNode : SimpleEdge -> String
targetNode (SimpleEdge source target label) =
    target


nodeNameList : List SimpleEdge -> List String
nodeNameList edgeList =
    (List.map sourceNode edgeList ++ List.map targetNode edgeList)
        |> unique
        |> List.sort


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
