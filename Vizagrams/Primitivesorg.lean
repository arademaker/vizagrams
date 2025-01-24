import scilean

namespace Primitives

structure Circle where
  r : Float
  c : Float^[2]

structure Line where
  pts : (Float^[2]) × (Float^[2])
