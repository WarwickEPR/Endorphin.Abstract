#### 0.3.2 - 2016-09-08
* Fix floating point errors in Vector.angle allowing for NaN to be returned if
  the two vectors were very close, but not exactly equal.

#### 0.3.1 - 2016-09-08
* Fix Azimuth angles in vectors being measured from the wrong axis.  They are
  now measured as 0 on the x-axis, pi/2 on the y-axis, etc.

#### 0.3.0 - 2016-08-01
* General functions for creating rectangular paths.

#### 0.2.5 - 2016-07-20
* Make pretty printing of points and vectors work with %A too.

#### 0.2.4 - 2016-07-20
* Add pretty printing of points and vectors
* Add map2 functions for points and vectors
* Add Point.midpoint function

#### 0.2.3 - 2016-07-08
* Add trigonometric and abs functions to angles.

#### 0.2.2 - 2016-07-08
* Add accessors for radians and degrees to angles.
* Add binding functions for angles to bind between pi and -pi.

#### 0.2.1 - 2016-07-05
* Add Point.getOrigin<'T> () to get an origin with a specified type.

#### 0.2.0 - 2016-07-05
* Add Endorphin.Abstract.Geometry namespace
** Add generic representation of Angles using either degrees or radians
** Add generic representation of 3-vectors and points in 3D space in arbitrary units of measure.

#### 0.1.1 - 2016-07-01
* Update to new API for Endorphin.Core

#### 0.1.0-beta2 - 2016-06-28
* Initial open-source release of Endorphin.Abstract
