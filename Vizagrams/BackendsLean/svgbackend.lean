import Lean.Data.Xml
import Vizagrams.Primitives -- Assumindo que este módulo contém suas estruturas de primitivas

open Lean Xml
open Primitive


class PrimToSvg (α : Type) where
  primToSvg : α → Lean.Xml.Element

instance : PrimToSvg (circle Nat) where
  primToSvg (c : circle Nat) : Lean.Xml.Element :=
    Lean.Xml.Element.Element "circle"
    (RBMap.fromList
      [
        ("cx", toString c.center.1),
        ("cy", toString c.center.2),
        ("r", toString c.radious)
      ]
    compare)
    #[]

instance : PrimToSvg (rectangle Nat) where
  primToSvg (r : rectangle Nat) : Lean.Xml.Element :=
    Lean.Xml.Element.Element "rect"
    (RBMap.fromList [
        ("x", toString r.origin.1),
        ("y", toString r.origin.2),
        ("width", toString r.width),
        ("height", toString r.height)
      ] compare)
    #[]

instance : PrimToSvg (ellipse Nat) where
  primToSvg (e : ellipse Nat) : Lean.Xml.Element :=
    Lean.Xml.Element.Element "ellipse"
    (RBMap.fromList [
        ("cx", toString e.center.1),
        ("cy", toString e.center.2),
        ("rx", toString e.rx),
        ("ry", toString e.ry)
      ] compare)
    #[]

instance : PrimToSvg (line Nat) where
  primToSvg (l : line Nat) : Lean.Xml.Element :=
    Lean.Xml.Element.Element "line"
    (RBMap.fromList [
        ("x1", toString l.starting.1),
        ("y1", toString l.starting.2),
        ("x2", toString l.ending.1),
        ("y2", toString l.ending.2)
      ] compare)
    #[]

instance : PrimToSvg (polyline Nat) where
  primToSvg (p : polyline Nat) : Lean.Xml.Element :=
    Lean.Xml.Element.Element "polyline"
    (RBMap.fromList [
        ("points", String.intercalate " " (p.points.map (λ (x, y) => s!"{x},{y}")))
      ] compare)
    #[]

instance : PrimToSvg (polygon Nat) where
  primToSvg (p : polygon Nat) : Lean.Xml.Element :=
    Lean.Xml.Element.Element "polygon"
    (RBMap.fromList [
        ("points", String.intercalate " " (p.points.map (λ (x, y) => s!"{x},{y}")))
      ] compare)
    #[]
