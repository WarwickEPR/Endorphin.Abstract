// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Abstract.Geometry

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// A representation of an angle.
[<CustomEquality ; CustomComparison>]
type Angle =
    private
    | Angle_deg of angle : float<deg>
    | Angle_rad of angle : float
    with
    /// Number of degrees per radian with type annotation.
    static member private deg_per_rad = (180.0 / Math.PI) * 1.0<deg>
    /// Number of radians per degree with type annotation.
    static member private rad_per_deg = 1.0 / Angle.deg_per_rad
    member private this.double_rad =
        match this with
        | Angle_deg angle -> angle * Angle.rad_per_deg
        | Angle_rad angle -> angle

    /// Get the hash code of an angle, which is shared with any other angle of the same size (regardless of unit).
    override this.GetHashCode () = this.double_rad.GetHashCode()
    /// Compare this angle to another angle, which returns true if the two have the same size (regardless of unit).
    override this.Equals angle =
        match angle with
        | :? Angle as other -> this.double_rad = other.double_rad
        | _                 -> false
    interface System.IComparable<Angle> with
        /// Compare this angle to another angle as size regardless of unit.
        member this.CompareTo angle = compare this.double_rad angle.double_rad
    interface System.IComparable with
        /// Compare this angle to another angle as size regardless of unit.
        member this.CompareTo angle =
            match angle with
            | :? Angle as other -> compare this.double_rad other.double_rad
            | _                 -> -1

    /// Create an Angle from a measurement in degrees.
    static member from_deg degrees = Angle_deg degrees
    /// Create an Angle from a measurement in radians.
    static member from_rad radians = Angle_rad radians

    /// Get the value of this angle in degrees.
    member this.to_deg =
        match this with
        | Angle_deg angle -> angle
        | Angle_rad angle -> angle * Angle.deg_per_rad

    /// Get the value of this angle in radians.
    member this.to_rad =
        match this with
        | Angle_deg angle -> angle * Angle.rad_per_deg
        | Angle_rad angle -> angle

    static member inline (+) (a1 : Angle, a2 : Angle) = Angle.from_deg <| a1.to_deg + a2.to_deg
    static member inline (-) (a1 : Angle, a2 : Angle) = Angle.from_deg <| a1.to_deg - a2.to_deg
    static member inline (*) (a1 : Angle, s) = Angle.from_deg <| a1.to_deg * s
    static member inline (*) (s, a1 : Angle) = Angle.from_deg <| a1.to_deg * s
    static member inline (/) (a1 : Angle, s) = Angle.from_deg <| a1.to_deg / s
    static member inline (/) (s, a1 : Angle) = Angle.from_deg <| a1.to_deg / s

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Angle =
    /// Create an angle using a value in degrees.
    let create x = Angle.from_deg x

    /// Create an angle using a value in radians (dimensionless float).
    let create_rad x = Angle.from_rad x
