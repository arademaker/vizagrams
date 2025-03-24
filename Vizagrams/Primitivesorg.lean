import scilean

namespace Primitives

structure Circle where
  r : Float
  c : Float^[2]

structure Line where
  pts : (Float^[2]) × (Float^[2])

structure Polygon where
  pts : Array (Float^[2])

structure Polyline where
  pts : Array (Float^[2])

structure Ellipse where
  rx : Float -- Semi-major axis
  ry : Float -- Semi-minor axis
  c : Float^[2] -- Center of the ellipse
  ang : Float
