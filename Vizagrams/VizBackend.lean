--import Vizagrams.VizPrim
import ProofWidgets.Data.Svg
import Vizagrams.Style
import Vizagrams.Envelope
import Vizagrams.FreeMonad

open ProofWidgets Svg
open GeometricPrimitive
open GraphicalPrimitive
open Sty
open FreeMonad
namespace VizBackend

private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 10
  width  := 400
  height := 400

def vecToPoint (x : Vec2) (fr : Frame) : Point fr :=
  Point.abs (x 0) (x 1)

def pointToVec {fr : Frame} (p : Point fr) : Vec2 :=
  match Point.toAbsolute p with
    | (x, y) => ![x,y]

def geomToShape (g : Geom) (fr : Frame) : Shape fr :=
  match g with
  | .line src trg =>
      Shape.line (vecToPoint src fr) (vecToPoint trg fr)
  | .circle r c =>
      Shape.circle (vecToPoint c fr) (Size.abs r)
  | .ellipse rx ry c =>
      Shape.ellipse (vecToPoint c fr) (Size.abs rx) (Size.abs ry)
  | .rect corner width height =>
      Shape.rect (vecToPoint corner fr) (Size.abs width) (Size.abs height)
  | .polyline points =>
      Shape.polyline (points.map (vecToPoint · fr))
  | .polygon points =>
      Shape.polygon (points.map (vecToPoint · fr))
  | .path d =>
      Shape.path d
  | .text pos content size =>
      Shape.text (vecToPoint pos fr) content (Size.abs size)


def primToElem (p : Prim) (fr : Frame) : Element fr :=
  { shape := geomToShape p.geom fr
  , fillColor := p.style.fillColor
  , strokeColor := p.style.strokeColor
  , strokeWidth := styleToSize p.style.strokeWidth fr
  }

def drawsvg (a : Array Prim) (fr : Frame := frame) : ProofWidgets.Html :=
  let svg : ProofWidgets.Svg fr := { elements := Array.map (λx => primToElem x fr) a}
  svg.toHtml

def draw (t : 𝕋 GraphicalMark.Mark) (fr : Frame := frame) : ProofWidgets.Html :=
  drawsvg (flat t ) fr

def NewCircle (r : Float) (c : Vec2) (st : Style := {fillColor := Color.mk 0 0 0}) : Prim :=
  let rc := Geom.circle r c
  {geom := rc, style := st}

def NewPolygon (pts : Array (Vec2)) (st : Style := {fillColor := Color.mk 0 0 0}) : Prim :=
  let p := Geom.polygon pts
  {geom := p , style := st}

def NewLine ( l₁ l₂  : Vec2 ) ( st : Style := {strokeColor := Color.mk 0 0 0} ): Prim :=
  let line := Geom.line l₁ l₂
  {geom := line , style := st}

def BoundingBox.toFrame (bb : Envelope.BoundingBox) : Frame :=
  let dx := (bb.upper 0) - (bb.lower 0)
  let dy := (bb.upper 1) - (bb.lower 1)
  let aspect := frame.height.toFloat / frame.width.toFloat
  let xSize' := max dx (dy / aspect)
  { frame with
    xmin  := bb.lower 0,
    ymin  := bb.lower 1,
    xSize := xSize' }

end VizBackend
