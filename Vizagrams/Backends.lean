import Vizagrams.Primitives

namespace SVG
open Primitive

class Primitive (α : Type) where
  draw : α → String

instance : Primitive (circle Nat) where
  draw p := s!"<svg> <circle radios='{p.radious}'>"

instance : Primitive (rectangle Nat) where
  draw p := s!"<svg> <rectangle radios='{p.a}'>"



end SVG
