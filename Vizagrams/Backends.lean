import Vizagrams.Primitives

namespace SVG
open Primitive

class Primitive (α : Type) where
  draw : α → String

instance : Primitive (circle Nat) where
  draw p :=
    s!"<svg> <circle r='{p.radious}' cx='{p.center.1}' cy='{p.center.2}' fill='none' stroke='black' /> </svg>"

instance : Primitive (rectangle Nat) where
  draw p :=
    s!"<svg> <rect x='{p.origin.1}' y='{p.origin.2}' width='{p.a}' height='{p.b}' fill='none' stroke='black' /> </svg>"

instance : Primitive (line Nat) where
  draw p :=
    s!"<svg> <line x1='{p.start.1}' y1='{p.start.2}' x2='{p.end_.1}' y2='{p.end_.2}' stroke='black' /> </svg>"

instance : Primitive (ellipse Nat) where
  draw p :=
    s!"<svg> <ellipse cx='{p.center.1}' cy='{p.center.2}' rx='{p.rx}' ry='{p.ry}' fill='none' stroke='black' /> </svg>"

instance : Primitive (polyline Nat) where
  draw p :=
    let points_str := String.intercalate " " (p.points.map (λ (x, y) => s!"{x},{y}"))
    s!"<svg> <polyline points='{points_str}' fill='none' stroke='black' /> </svg>"

instance : Primitive (polygon Nat) where
  draw p :=
    let points_str := String.intercalate " " (p.points.map (λ (x, y) => s!"{x},{y}"))
    s!"<svg> <polygon points='{points_str}' fill='none' stroke='black' /> </svg>"
/-
instance : Primitive (path Nat) where
  draw p :=
    s!"<svg> <path d='{p.d}' fill='none' stroke='black' /> </svg>"
-/
end SVG
