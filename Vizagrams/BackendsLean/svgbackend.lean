import Lean.Data.Xml
import Vizagrams.Primitives -- Assumindo que este módulo contém suas estruturas de primitivas

open Lean Xml
open Primitive


/-
def placeNextTo (c : circle Float) (r : rectangle Float) (direction : Float × Float) : rectangle Float :=
  let envelopeC := envelope_circle c direction
  let envelopeR := envelope_rectangle r (direction.1 * -1, direction.2 * -1)  -- Envelope do retângulo na direção oposta
  let shift := envelopeC - envelopeR
  { r with origin := (r.origin.1 + shift * direction.1, r.origin.2 + shift * direction.2) }
-/

-- Função para posicionar `c2` ao lado de `c1` na direção `direction`
def placeCircleNextTo (c1 : circle Float) (c2 : circle Float) (direction : Float × Float) : circle Float :=
  let v_norm := normalize direction  -- Normalizar o vetor de direção
  let distance := c1.radious + c2.radious  -- Distância necessária entre os centros
  let new_center := (c1.center.1 + v_norm.1 * distance, c1.center.2 + v_norm.2 * distance)
  { c2 with center := new_center }



def placeRectangleNextToCircle (c : circle Float) (r : rectangle Float) (direction : Float × Float) : rectangle Float :=
  let v_norm := normalize direction  -- Normalizar a direção
  let distance := c.radious + envelope_rectangle r (-v_norm.1, -v_norm.2)  -- Soma da distância do círculo e do envelope oposto do retângulo
  let new_origin := (c.center.1 + v_norm.1 * distance, c.center.2 + v_norm.2 * distance - r.height / 2)
  { r with origin := new_origin }

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


def combineSvgElements (elements : List Lean.Xml.Element) : Lean.Xml.Element :=
  let contents : Array Content := elements.map Content.Element |>.toArray
  Lean.Xml.Element.Element "svg"
    (RBMap.fromList [("xmlns", "http://www.w3.org/2000/svg"), ("width", "200"), ("height", "100")] compare)
    contents


def myCircle : circle Float := { center := (50.0, 50.0), radious := 25.0 }
def myRectangle : rectangle Float := { origin := (0.0, 0.0), width := 50.0, height := 30.0 }

#eval PrimToSvg.primToSvg myCircle
def positionedRectangle := placeRectangleNextToCircle myCircle myRectangle (0.0, 1.0)

#eval PrimToSvg.primToSvg positionedRectangle

def positionedRectangleXml := PrimToSvg.primToSvg positionedRectangle

#eval (PrimToSvg.primToSvg myCircle)
#eval (PrimToSvg.primToSvg positionedRectangle)

#eval positionedRectangle

def svgImage : Lean.Xml.Element :=
  combineSvgElements [
    PrimToSvg.primToSvg myCircle,
    PrimToSvg.primToSvg positionedRectangle
  ]

#eval svgImage

def svgElement : Lean.Xml.Element :=
  Lean.Xml.Element.Element "svg"
    (RBMap.fromList [("xmlns", "http://www.w3.org/2000/svg"), ("width", "200"), ("height", "100")] compare)
    #[Content.Element (PrimToSvg.primToSvg myCircle), Content.Element (PrimToSvg.primToSvg positionedRectangle)]

#eval svgElement

def c1 : circle Float := { center := (50.0, 50.0), radious := 25.0 }
def c2 : circle Float := { center := (0.0, 0.0), radious := 15.0 }

-- Posiciona `c2` à direita de `c1`
def positionedCircle := placeCircleNextTo c1 c2 (1.0, 1.0)

#eval positionedCircle

def svgElement1 : Lean.Xml.Element :=
  Lean.Xml.Element.Element "svg"
    (RBMap.fromList [("xmlns", "http://www.w3.org/2000/svg"), ("width", "200"), ("height", "100")] compare)
    #[Content.Element (PrimToSvg.primToSvg c1), Content.Element (PrimToSvg.primToSvg positionedCircle)]

#eval svgElement1
