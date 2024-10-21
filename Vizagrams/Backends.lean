import Vizagrams.Primitives

namespace SVG
open Primitive

class Primitive (α : Type) where
  draw : α → String

instance : Primitive (circle Nat) where
  draw p :=
    s!"<svg><circle cx='{p.center.1}' cy='{p.center.2}' r='{p.radious}' /></svg>"

instance : Primitive (rectangle Nat) where
  draw p :=
    s!"<svg><rect x='{p.origin.1}' y='{p.origin.2}' width='{p.width}' height='{p.height}'/></svg>"

instance : Primitive (ellipse Nat) where
  draw p :=
    s!"<svg><ellipse cx='{p.center.1}' cy='{p.center.2}' rx='{p.rx}' ry='{p.ry}'/></svg>"

instance : Primitive (line Nat) where
  draw p :=
    s!"<svg><line x1='{p.starting.1}' y1='{p.starting.2}' x2='{p.ending.1}' y2='{p.ending.2}'stroke='black' /></svg>"

instance : Primitive (polyline Nat) where
  draw p :=
    let points_str := String.intercalate " " (p.points.map (λ (x, y) => s!"{x},{y}"))
    s!"<svg><polyline points='{points_str}' fill='none' stroke='black' /></svg>"

instance : Primitive (polygon Nat) where
  draw p :=
    let points_str := String.intercalate " " (p.points.map (λ (x, y) => s!"{x},{y}"))
    s!"<svg><polygon points='{points_str}' fill='none' stroke='black' />-</svg>"


end SVG
