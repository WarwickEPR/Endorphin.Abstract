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
        /// create path i.e. x0 {y0 -> yn}; x1 {y0 -> yn}; x2 {y0 -> yn} etc
        let create origin (gridSize : decimal<um> * decimal<um>) (stepSize : decimal<um> * decimal<um>) plane =
            let numberOfStepsX = int (round ((float ((fst gridSize)/1m<um>)) / (float ((fst stepSize)/1.0m<um>))))
            let numberOfStepsY = int (round ((float ((snd gridSize)/1m<um>)) / (float ((snd stepSize)/1.0m<um>))))

            let path = seq {
                for x in 0 .. numberOfStepsX do
                    for y in 0 .. numberOfStepsY -> (x, y) }

            { ArrayIndices = path |> Seq.toArray
              Origin = origin
              Scale = (fst stepSize, snd stepSize)
              Plane = plane }

        /// create path by number of points i.e. x0 {y0 -> yn}; x1 {y0 -> yn}; x2 {y0 -> yn} etc
        let createByNumberOfPoints origin (gridSize : decimal<um> * decimal<um>) (numberOfPoints : int * int) plane =
            let (numberOfPointsX, numberOfPointsY) = numberOfPoints

            let path = seq {
                for x in 0 .. (numberOfPointsX - 1) do
                    for y in 0 .. (numberOfPointsY - 1) -> (x, y) }

            let stepSizeX = 1m<um> * (((fst gridSize) / 1m<um>) / (decimal <| fst numberOfPoints))
            let stepSizeY = 1m<um> * (((snd gridSize) / 1m<um>) / (decimal <| snd numberOfPoints))

            { ArrayIndices = path |> Seq.toArray
              Origin = origin
              Plane = plane
              Scale = (stepSizeX, stepSizeY) }

        /// create snaked path i.e. x0 {y0 -> yn}; x1 {yn -> y0}; x2 {y0 -> yn} etc
        let createSnake origin (gridSize : decimal<um> * decimal<um>) (stepSize : decimal<um> * decimal<um>) plane =
            let isEven x = (x % 2 = 0)

            let numberOfStepsX = int (round ((float ((fst gridSize)/1m<um>)) / (float ((fst stepSize)/1.0m<um>))))
            let numberOfStepsY = int (round ((float ((snd gridSize)/1m<um>)) / (float ((snd stepSize)/1.0m<um>))))

            let path = seq {
                for y in 0 .. numberOfStepsY do
                    if isEven y
                    then for x in 0 .. numberOfStepsX -> (x, y)
                    else for x in numberOfStepsX .. -1 .. 0 -> (x, y) }

            { ArrayIndices = path |> Seq.toArray
              Origin = origin
              Plane = plane
              Scale = (fst stepSize, snd stepSize) }

        /// create snaked path by number of points i.e. x0 {y0 -> yn}; x1 {yn -> y0}; x2 {y0 -> yn} etc
        let createSnakeByNumberOfPoints origin (gridSize : decimal<um> * decimal<um>) (numberOfPoints : int * int) plane =
            let isEven x = (x % 2 = 0)

            let (numberOfPointsX, numberOfPointsY) = numberOfPoints

            let path = seq {
                for y in 0 .. (numberOfPointsY - 1) do
                    if isEven y
                    then for x in 0 .. (numberOfPointsX - 1) -> (x, y)
                    else for x in (numberOfPointsX - 1) .. -1 .. 0 -> (x, y) }

            let stepSizeX = 1m<um> * (((fst gridSize) / 1m<um>) / (decimal <| fst numberOfPoints))
            let stepSizeY = 1m<um> * (((snd gridSize) / 1m<um>) / (decimal <| snd numberOfPoints))

            { ArrayIndices = path |> Seq.toArray
              Origin = origin
              Plane = plane
              Scale = (stepSizeX, stepSizeY) }

        /// create square path i.e. x0 {y0 -> yn}; x1 {y0 -> yn}; x2 {y0 -> yn} etc
        let createSquare origin gridSize stepSize plane =
            create origin (gridSize, gridSize) (stepSize, stepSize) plane

         /// create square path by number of points i.e. x0 {y0 -> yn}; x1 {y0 -> yn}; x2 {y0 -> yn} etc
        let createSquareByNumberOfPoints origin gridSize stepSize plane =
            createByNumberOfPoints origin (gridSize, gridSize) (stepSize, stepSize) plane

        /// create square snaked path i.e. x0 {y0 -> yn}; x1 {yn -> y0}; x2 {y0 -> yn} etc
        let createSquareSnake origin gridSize stepSize plane =
            createSnake origin (gridSize, gridSize) (stepSize, stepSize) plane

        /// create square snaked path by number of points i.e. x0 {y0 -> yn}; x1 {yn -> y0}; x2 {y0 -> yn} etc
        let createSquareSnakeByNumberOfPoints origin gridSize numberOfPoints plane =
            createSnakeByNumberOfPoints origin (gridSize, gridSize) (numberOfPoints, numberOfPoints) plane

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