module FlowModel
    exposing
        ( efficiency
        , resilience
        , alpha
        , sustainability
        , sustainabilityPercentage
        )

import SimpleGraph as SG exposing (SimpleGraph, Edge)
import Graph


efficiency : SimpleGraph -> Float
efficiency graph =
    let
        totalFlow_ =
            SG.totalFlow graph

        edges =
            Graph.edges graph
    in
        List.map (efficiencyOfEdge totalFlow_ graph) edges
            |> List.sum
            |> roundTo 3


resilience : SimpleGraph -> Float
resilience graph =
    let
        totalFlow_ =
            SG.totalFlow graph

        edges =
            Graph.edges graph
    in
        List.map (resilienceOfEdge totalFlow_ graph) edges
            |> List.sum
            |> \x -> -(roundTo 3 x)


alpha : SimpleGraph -> Float
alpha graph =
    let
        ratio =
            1 + ((resilience graph) / (efficiency graph))
    in
        1 / ratio


sustainability : SimpleGraph -> Float
sustainability graph =
    let
        a =
            alpha graph

        aa =
            a ^ 1.288

        s =
            -1.844 * aa * (logBase 2 aa)
    in
        roundTo 4 s


sustainabilityPercentage : SimpleGraph -> Float
sustainabilityPercentage graph =
    roundTo 2 (100 * (sustainability graph))


efficiencyOfEdge : Float -> SimpleGraph -> Edge -> Float
efficiencyOfEdge totalFlow_ graph edge =
    let
        denominator =
            (SG.outFlow edge.from graph) * (SG.inFlow edge.to graph)

        numerator =
            edge.label * totalFlow_

        logRatio =
            (logBase 2) (numerator / denominator)
    in
        roundTo 3 (edge.label * logRatio)


resilienceOfEdge : Float -> SimpleGraph -> Edge -> Float
resilienceOfEdge totalFlow_ graph edge =
    let
        denominator =
            (SG.outFlow edge.from graph) * (SG.inFlow edge.to graph)

        numerator =
            edge.label * edge.label

        logRatio =
            (logBase 2) (numerator / denominator)
    in
        edge.label * logRatio


roundTo : Int -> Float -> Float
roundTo places quantity =
    let
        factor =
            10 ^ places

        ff =
            (toFloat factor)

        q2 =
            ff * quantity

        q3 =
            round q2
    in
        (toFloat q3) / ff
