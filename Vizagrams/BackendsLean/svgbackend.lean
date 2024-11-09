import Lean.Data.Xml
import Vizagrams.Primitives -- Assumindo que este módulo contém suas estruturas de primitivas

open Lean Xml
open Primitive

-- Função para posicionar `c2` ao lado de `c1` na direção `direction`
def placeCircleNextTo (c1 : circle Float) (c2 : circle Float) (direction : Float × Float) : circle Float :=
  let v_norm := normalize direction  -- Normalizar o vetor de direção
  let distance := c1.radious + c2.radious  -- Distância necessária entre os centros
  let new_center := (c1.center.1 + v_norm.1 * distance, c1.center.2 + v_norm.2 * distance)
  { c2 with center := new_center }

-- Inacabado
def placeRectangleNextToCircle (c : circle Float) (r : rectangle Float) (direction : Float × Float) : rectangle Float :=
  let v_norm := normalize direction  -- Normalizar a direção
  let distance := c.radious + envelope_rectangle r (-v_norm.1, -v_norm.2)  -- Soma da distância do círculo e do envelope oposto do retângulo
  let new_origin := (c.center.1 + v_norm.1 * distance, c.center.2 + v_norm.2 * distance - r.height / 2)
  { r with origin := new_origin }

-- Classe que retorna xml elementos
class PrimToSvg (α : Type) where
  primToSvg : α → Lean.Xml.Element

instance : PrimToSvg (circle Float ) where
  primToSvg (c : circle Float ) : Lean.Xml.Element :=
    Lean.Xml.Element.Element "circle"
    (RBMap.fromList
      [
        ("cx", s!"{c.center.1}"),
        ("cy", s!"{c.center.2}"),
        ("r", s!"{c.radious}")
      ]
    compare)
    #[]

instance : PrimToSvg (rectangle Float) where
  primToSvg (r : rectangle Float) : Lean.Xml.Element :=
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
