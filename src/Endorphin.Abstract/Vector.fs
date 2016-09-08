// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Abstract.Geometry

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// A Cartesian type, holding a point in Cartesian (x, y, z) format.
[<StructuredFormatDisplay("({X}, {Y}, {Z})")>]
type Vector<[<Measure>] 'Unit> =
    { X : float<'Unit>
      Y : float<'Unit>
      Z : float<'Unit> }
    with
    override this.ToString () = sprintf "(%g, %g, %g)" this.X this.Y this.Z
    /// Get the magnitude of this vector.
    member this.Magnitude = sqrt (this.X * this.X + this.Y * this.Y + this.Z * this.Z)
    /// Get the equivalent spherical inclination of this vector.
    member this.Inclination = acos (this.Z / this.Magnitude) |> Angle.from_rad
    /// Get the equivalent spherical azimuth of this vector.
    member this.Azimuth = atan2 this.Y this.X |> Angle.from_rad

    /// Add one vector to another.
    static member inline (+) (v1, v2) =
        { X = v1.X + v2.X ; Y = v1.Y + v2.Y ; Z = v1.Z + v2.Z }
    /// Subtract one vector from another.
    static member inline (-) (v1, v2) =
        { X = v1.X - v2.X ; Y = v1.Y - v2.Y ; Z = v1.Z - v2.Z }
    /// Multiply a vector by a scalar.
    static member inline (*) (s, v) =
        { X = s * v.X ; Y = s * v.Y ; Z = s * v.Z }
    /// Multiply a vector by a scalar.
    static member inline (*) (v, s) =
        { X = s * v.X ; Y = s * v.Y ; Z = s * v.Z }
    /// Divide a vector by a scalar.
    static member inline (/) (s, v) =
        { X = v.X / s ; Y = v.Y / s ; Z = v.Z / s }
    /// Divice a vector by a scalar.
    static member inline (/) (v, s) =
        { X = v.X / s ; Y = v.Y / s ; Z = v.Z / s}
    /// Dot (scalar) product of two vectors.
    static member inline ( .* ) (v1, v2) =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z
    /// Cross (vector) product of two vectors.
    static member inline ( ^* ) (v1, v2) =
        { X = v1.Y * v2.Z - v1.Z * v2.Y
          Y = v1.Z * v2.X - v1.X * v2.Z
          Z = v1.X * v2.Y - v1.Y * v2.X }
    /// Unary negation of a vector.
    static member inline ( ~- ) v1 =
        { X = -v1.X ; Y = -v1.Y ; Z = -v1.Z }

/// A representation of a point in space.
[<StructuredFormatDisplay("({X}, {Y}, {Z})")>]
type Point<[<Measure>] 'Unit> = private Point of Vector<'Unit>
    with
    /// Create a Point from a Cartesian location.
    static member internal fromVector : Vector<'Unit> -> Point<'Unit> = Point
    override this.ToString () = sprintf "(%g, %g, %g)" this.X this.Y this.Z

    /// Get the x co-ordinate of the Point.
    member this.X = match this with Point vec -> vec.X
    /// Get the y co-ordinate of the Point.
    member this.Y = match this with Point vec -> vec.Y
    /// Get the z co-ordinate of the Point.
    member this.Z = match this with Point vec -> vec.Z

    /// Get the spherical radius of the Point.
    member this.Radius = match this with Point vec -> vec.Magnitude
    /// Get the spherical inclination angle of the Point.
    member this.Inclination = match this with Point vec -> vec.Inclination
    /// Get the spherical azimuth angle of the Point.
    member this.Azimuth = match this with Point vec -> vec.Azimuth

    /// The difference between two Points as a vector.
    static member inline (-) (p1 : Point<'T>, p2 : Point<'T>) =
        { X = p1.X - p2.X ; Y = p1.Y - p2.Y ; Z = p1.Z - p2.Z }

    /// The Point representing (0, 0, 0) in the correct units discovered by type inference.
    static member Origin =
        Point { X = LanguagePrimitives.FloatWithMeasure<'Unit> 0.0 ; Y = 0.0<_> ; Z = 0.0<_> }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    /// Create a vector from a set of Cartesian base vectors.
    let create (x : float<'T>) y z =
        { X = x ; Y = y ; Z = z }

    /// Create a vector from a set of spherical co-ordinates.
    let createSpherical (radius : float<'T>) (inclination : Angle) (azimuth : Angle) =
        { X = radius * sin (inclination.to_rad) * cos (azimuth.to_rad)
          Y = radius * sin (inclination.to_rad) * sin (azimuth.to_rad)
          Z = radius * cos (inclination.to_rad) }

    /// Get the x value of a Vector.
    let x (vec : Vector<'T>) = vec.X
    /// Get the y value of a Vector.
    let y (vec : Vector<'T>) = vec.Y
    /// Get the z value of a Vector.
    let z (vec : Vector<'T>) = vec.Z

    /// Get a 3-tuple of the co-ordinates of the vector.
    let tuple vec = (x vec, y vec, z vec)

    /// Create a new vector by mapping the old values in a 3-tuple.
    let mapEach mapper vec =
        let (x, y, z) = mapper (tuple vec)
        create x y z

    /// Create a new vector by mapping two vectors in 3-tuples.
    let mapEach2 mapper v1 v2 =
        let (x, y, z) = mapper (tuple v1) (tuple v2)
        create x y z

    /// Create a new vector by mapping each individual co-ordinate with the same mapping
    /// function.
    let map mapper vec =
        create (mapper vec.X) (mapper vec.Y) (mapper vec.Z)

    /// Create a new vector by mapping each individual co-ordinate with the same mapping
    /// function.
    let map2 mapper v1 v2 =
        create (mapper v1.X v2.X) (mapper v1.Y v2.Y) (mapper v1.Z v2.Z)

    /// Create a new vector by apply the mapper to each element separately, with a counter to
    /// indicate which co-ordinate is being fed in. (x : 0, y : 1, z : 2).
    let mapi mapper vec =
        let x = mapper 0 vec.X
        let y = mapper 1 vec.Y
        let z = mapper 2 vec.Z
        create x y z

    /// Create a new vector by apply the mapper to each element of two vectors separately
    /// but pairwise, with a counter to indicate which co-ordinate is being fed in.
    /// (x : 0, y : 1, z : 2).
    let mapi2 mapper v1 v2 =
        let x = mapper 0 v1.X v2.X
        let y = mapper 1 v1.Y v2.Y
        let z = mapper 2 v1.Z v2.Z
        create x y z

    /// Get the vector that transforms one point into another.
    let from (source : Point<'T>) destination = destination - source

    /// Get the position vector representation of a point.
    let ofPoint (Point point) = point

    /// Get the magnitude of a vector.
    let magnitude (vec : Vector<'T>) = vec.Magnitude

    /// Create a unit vector in the same direction as the given vector.
    let unit (vec : Vector<'T>) =
        // cast magnitude to float to conserve units of measure
        map (fun x -> x / float vec.Magnitude) vec

    /// Get the dot product of two vectors.  Also available as an operator .* in the Vector type.
    let dot (v1 : Vector<'T>) v2 = v1 .* v2

    /// Get the cross product of two vectors.  Also available as the operator ^* in the Vector type.
    let cross (v1 : Vector<'T>) v2 = v1 ^* v2

    /// Change the x value of a vector.
    let withX x vec = { vec with X = x }
    /// Change the y value of a vector.
    let withY y vec = { vec with Y = y }
    /// Change the z value of a vector.
    let withZ z vec = { vec with Z = z }

    /// Get the angle between 2 vectors.
    let angle (v1 : Vector<'u>) (v2 : Vector<'v>) =
        let mag = magnitude v1 * magnitude v2
        v1 .* v2 / mag |> acos |> Angle.create_rad

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Point =
    /// Create a Point from a set of Cartesian co-ordinates.
    let create x y z =
        Point.fromVector <| Vector.create x y z

    /// Extract the vector from a Point, apply the genericised vector function and return a Point.
    let private vecPassthrough func (Point point) = Point.fromVector <| func point

    /// Extract the vector from a Point, apply the genericised vector function and return a Point.
    let private vecPassthrough2 func (Point p1) (Point p2) = Point.fromVector <| func p1 p2

    /// Map each co-ordinate of a point using the same mapping function.
    let map mapper point = vecPassthrough (Vector.map mapper) point

    /// Map each co-ordinate of two points using the same mapping function.
    let map2 mapper p1 p2 = vecPassthrough2 (Vector.map2 mapper) p1 p2

    /// Map each co-ordinate of a point using the same mapping function, with an index indicating
    /// which co-ordinate was passed (x : 0, y : 1, z : 2).
    let mapi mapper point = vecPassthrough (Vector.mapi mapper) point

    /// Map each co-ordinate of two points using the same mapping function, with an index indicating
    /// which co-ordinate was passed (x : 0, y : 1, z : 2).
    let mapi2 mapper p1 p2 = vecPassthrough2 (Vector.mapi2 mapper) p1 p2

    /// Map a point using a function which accepts and returns a 3-tuple (x, y, z) of the co-ordinates.
    let mapEach mapper point = vecPassthrough (Vector.mapEach mapper) point

    /// Map two points using a function which accepts and returns a 3-tuple of the co-ordinates.
    let mapEach2 mapper p1 p2 = vecPassthrough2 (Vector.mapEach2 mapper) p1 p2

    /// Get the x value of a Point.
    let x (point : Point<'T>) = point.X
    /// Get the y value of a Point.
    let y (point : Point<'T>) = point.Y
    /// Get the z value of a Point.
    let z (point : Point<'T>) = point.Z

    /// Get a 3-tuple (x, y, z) of the Point.
    let tuple point = (x point, y point, z point)

    /// Change the x co-ordinate of a point.
    let withX x point = vecPassthrough (Vector.withX x) point
    /// Change the y co-ordinate of a point.
    let withY y point = vecPassthrough (Vector.withY y) point
    /// Change the z co-ordinate of a point.
    let withZ z point = vecPassthrough (Vector.withZ z) point

    /// Get the spherical radius of a point.
    let radius (point : Point<'T>) = point.Radius
    /// Get the spherical inclination of a point.
    let inclination (point : Point<'T>) = point.Inclination
    /// Get the spherical azimuth of a point.
    let azimuth (point : Point<'T>) = point.Azimuth

    /// Flip the parity of a point in all 3 dimensions.
    let reverseParity point = map (~-) point

    /// Get the midpoint of two points.
    let midpoint (p1 : Point<'T>) p2 =
        map2 (fun a b -> (a + b) * 0.5) p1 p2

    /// Change the spherical radius of a point.
    let withRadius (radius : float<'T>) (point : Point<'T>) =
        Point.fromVector <| Vector.createSpherical radius point.Inclination point.Azimuth
    /// Change the spherical inclination of a point.
    let withInclination inclination (point : Point<'T>) =
        Point.fromVector <| Vector.createSpherical point.Radius inclination point.Azimuth
    /// Change the spherical azimuth of a point.
    let withAzimuth azimuth (point : Point<'T>) =
        Point.fromVector <| Vector.createSpherical point.Radius point.Inclination azimuth

    /// Get an origin (0, 0, 0) point with a specified type annotation.
    let getOrigin<[<Measure>] 'Unit> () : Point<'Unit> = Point.Origin