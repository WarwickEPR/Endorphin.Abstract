// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

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
            override x.ToString() = sprintf "%A" x

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

        /// create a path where imaging data is always taken in the same direction with "fly-back" points before the
        /// next row is imaged i.e. x0 {y0 -> yn} x1 {yn .. -10 .. y0} x1 {y0 -> yn}
        // Ideally we'd not dwell at each point on the flyback, but that would require scanner changes to set the dwell time per point
        let createRaster origin (gridSize : decimal<um> * decimal<um>) (stepSize : decimal<um> * decimal<um>) plane =
            
            let numberOfStepsX = int (floor ((float ((fst gridSize)/1m<um>)) / (float ((fst stepSize)/1.0m<um>))))
            let numberOfStepsY = int (floor ((float ((snd gridSize)/1m<um>)) / (float ((snd stepSize)/1.0m<um>))))
            let flybackStep = 1m<um>/(fst stepSize) |> round |> int

            let path = seq {
                for y in 0 .. (numberOfStepsY-1) do
                    if y <> 0 then for x in (numberOfStepsX-flybackStep) .. -flybackStep .. flybackStep -> (x,y) // flyback to within 1um of the start of the next row
                    for x in 0 .. (numberOfStepsX-1) -> (x,y) } // imaging points, may overwrite "flyback" points

            { ArrayIndices = path |> Seq.toArray
              Origin = origin
              Plane = plane
              Scale = (fst stepSize, snd stepSize) }

        /// create a path where imaging data is always taken in the same direction with "fly-back" points before the
        /// next row is imaged i.e. x0 {y0 -> yn} x1 {yn .. -10 .. y0} x1 {y0 -> yn}
        // Ideally we'd not dwell at each point on the flyback, but that would require scanner changes to set the dwell time per point
        let createRasterByNumberOfPoints origin (gridSize : decimal<um> * decimal<um>) (numberOfPoints : int * int) plane =

            let (numberOfPointsX, numberOfPointsY) = numberOfPoints
            let flybackStep = 1m<um>/(fst gridSize)*(numberOfPointsX |> decimal) |> round |> int

            let path = seq {
                for y in 0 .. (numberOfPointsY-1) do
                    if y <> 0 then for x in (numberOfPointsX-flybackStep) .. -flybackStep .. flybackStep -> (x,y) // flyback to within 1um of the start of the next row
                    for x in 0 .. (numberOfPointsX-1) -> (x,y) } // imaging points, may overwrite "flyback" points

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

        /// create square snaked path by number of points i.e. x0 {y0 -> yn}; x1 {yn .. -some .. y0}; x1 {y0 -> yn}; etc
        /// Imaging points are always taken in the same direction with sparse "flyback" points in between
        let createSquareSnakeByNumberOfPoints origin gridSize numberOfPoints plane =
            createSnakeByNumberOfPoints origin (gridSize, gridSize) (numberOfPoints, numberOfPoints) plane

        /// create square rastered path i.e. x0 {y0 -> yn}; x1 {yn -> y0}; x2 {y0 -> yn} etc
        let createSquareRaster origin gridSize stepSize plane =
            createRaster origin (gridSize, gridSize) (stepSize, stepSize) plane

        /// create square rastered path by number of points i.e. x0 {y0 -> yn}; x1 {yn -> y0}; x2 {y0 -> yn} etc
        let createSquareRasterByNumberOfPoints origin gridSize numberOfPoints plane =
            createRasterByNumberOfPoints origin (gridSize, gridSize) (numberOfPoints, numberOfPoints) plane

        let createOneDirection origin gridSize stepSize plane =
            let numberOfSteps = int (round((float gridSize) / (float (stepSize/1.0m<um>))))
            let path = seq {
                for x in 0 .. numberOfSteps do
                    for y in 0 .. numberOfSteps -> (x, y) }

            { ArrayIndices = path |> Seq.toArray
              Origin = origin
              Scale = (stepSize, stepSize)
              Plane = plane }

        /// Extract the real coordinate for a point in the context of a path
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

        /// Extract the points in a path
        let points path =
            path.ArrayIndices |> Array.ofSeq

        /// Extract a point from a path
        let pointAtIndex i path =
            path.ArrayIndices.[i]

        /// Extract the real coordinates for a point at a position in a path
        let coordinatesAtIndex i path =
            path |> coordinateForPoint path.ArrayIndices.[i]

        /// Create a modified path with the last point repeated
        let repeatLastPoint (path:Path) =
            { path with ArrayIndices = Array.append path.ArrayIndices [| Array.last path.ArrayIndices |] }

        /// Create a modified path with the first point repeated
        let repeatFirstPoint (path:Path) =
            { path with ArrayIndices = Array.append [| Array.head path.ArrayIndices |] path.ArrayIndices }
