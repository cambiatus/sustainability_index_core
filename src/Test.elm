module Test exposing (run, ex1, ex2)

import CSV
import FlowModel
import SimpleGraph


{-| Usage:

> import Test exposing(ex1, ex2)

> Test.run ex1
> { alpha = 0.4773071923743501, edgeCount = 4, efficiency = 8.813
> , nodeCount = 4, resilience = 9.651, sustainability = 0.9775, totalFlow = 10 }

> Test.run ex2
> { alpha = 0.3848187874515115, edgeCount = 13, efficiency = 621.709
> , nodeCount = 8, resilience = 993.88, sustainability = 0.9564, totalFlow = 456.2 }

-}
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
        |> CSV.graphFromString
        |> FlowModel.sustainability


efficiency : String -> Float
efficiency input =
    input
        |> CSV.graphFromString
        |> FlowModel.efficiency


resilience : String -> Float
resilience input =
    input
        |> CSV.graphFromString
        |> FlowModel.resilience


alpha : String -> Float
alpha input =
    input
        |> CSV.graphFromString
        |> FlowModel.alpha


totalFlow : String -> Float
totalFlow input =
    input
        |> CSV.graphFromString
        |> SimpleGraph.totalFlow


nodeCount : String -> Int
nodeCount input =
    input
        |> CSV.graphFromString
        |> SimpleGraph.nodeCount


edgeCount : String -> Int
edgeCount input =
    input
        |> CSV.graphFromString
        |> SimpleGraph.edgeCount


ex1 =
    "\nA,B,1\nA,C,2\nB,D,3\nC,D,4\n"


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
