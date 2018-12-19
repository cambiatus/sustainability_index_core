module Test exposing (..)

import CSV
import FlowModel
import SimpleGraph


type alias Report =
    { nodeCount : Int
    , edgeCount : Int
    , totalFlow : Float
    , sustainability : Float
    , efficiency : Float
    , resilience : Float
    , alpha : Float
    }


run : String -> Report
run input =
    { nodeCount = nodeCount input
    , edgeCount = edgeCount input
    , totalFlow = totalFlow input
    , sustainability = sustainability input
    , efficiency = efficiency input
    , resilience = resilience input
    , alpha = alpha input
    }


sustainability : String -> Float
sustainability input =
    input
        |> CSV.build
        |> FlowModel.sustainability


efficiency : String -> Float
efficiency input =
    input
        |> CSV.build
        |> FlowModel.efficiency


resilience : String -> Float
resilience input =
    input
        |> CSV.build
        |> FlowModel.resilience


alpha : String -> Float
alpha input =
    input
        |> CSV.build
        |> FlowModel.alpha


totalFlow : String -> Float
totalFlow input =
    input
        |> CSV.build
        |> SimpleGraph.totalFlow


nodeCount : String -> Int
nodeCount input =
    input
        |> CSV.build
        |> SimpleGraph.nodeCount


edgeCount : String -> Int
edgeCount input =
    input
        |> CSV.build
        |> SimpleGraph.edgeCount


ex1 =
    "  \nA,B,23\nA,C,10\nB,D,40\nC,D,5  \n"


ex2 =
    """
Lucca, Pablo, 30
Lucca, Karla, 90.4
Pablo, Ranulfo, 22
Karla, Luz, 40
Karla, Maria, 55
Karla, Ranulfo, 31.4
Jim, Karla, 30
Jim, Ranulfo, 20
Pablo, Karla, 34
Ranulfo, Lucca, 20
Luz, Maria, 22
George, Maria, 31.4
Lucca, Jim, 30
"""
