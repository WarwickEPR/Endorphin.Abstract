﻿// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Abstract

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module Position =
    module Point =
        type VoltagePoint = decimal<V> * decimal<V> * decimal<V>

        type Point = decimal<um> * decimal<um> * decimal<um>

        /// Get the first member of a 3-tuple.
        let tfst (a, _, _) = a
        /// Get the second member of a 3-tuple.
        let tsnd (_, a, _) = a
        /// Get the third member of a 3-tuple.
        let ttrd (_, _, a) = a

    module Path =
        open Point

        type Plane =
            | XY
            | XZ
            | YZ

        type Path =
            private { ArrayIndices : (int * int) array
                      Origin       : Point
                      Scale        : decimal<um> * decimal<um>
                      Plane        : Plane }

        let createSnake origin (gridSize : int<um>) stepSize plane =
            let isEven x = (x % 2 = 0)

            let numberOfSteps = int (round((float gridSize) / (float (stepSize/1.0m<um>))))
            let path = seq {
                for x in 0 .. numberOfSteps do
                    if isEven x
                    then for y in 0 .. numberOfSteps -> (x,y)
                    else for y in numberOfSteps .. -1 .. 0 -> (x,y) }

            { ArrayIndices = path |> Seq.toArray
              Origin = origin
              Plane = plane
              Scale = (stepSize, stepSize) }

        let createOneDirection origin gridSize stepSize plane =
            let numberOfSteps = int (round((float gridSize) / (float (stepSize/1.0m<um>))))
            let path = seq {
                for x in 0 .. numberOfSteps do
                    for y in 0 .. numberOfSteps -> (x, y) }

            { ArrayIndices = path |> Seq.toArray
              Origin = origin
              Scale = (stepSize, stepSize)
              Plane = plane }

        let coordinateForPoint (point : int * int) path =
            let (x0, y0, z0) = path.Origin

            match path.Plane with
            | XY ->
                let (x, y) = point
                let (dx, dy) = path.Scale
                (x0 + (decimal x) * dx, y0 + (decimal y * dy), z0)
            | XZ ->
                let (x, z) = point
                let (dx, dz) = path.Scale
                (x0 + (decimal x) * dx, y0, z0 + (decimal z * dz))
            | YZ ->
                let (y, z) = point
                let (dy, dz) = path.Scale
                (x0, y0 + (decimal y * dy), z0 + (decimal z * dz))

        let points path =
            path.ArrayIndices |> Array.ofSeq

        let pointAtIndex i path =
            path.ArrayIndices.[i]

        let coordinatesAtIndex i path =
            path |> coordinateForPoint path.ArrayIndices.[i]
