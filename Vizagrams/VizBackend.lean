--import Vizagrams.VizPrim
import ProofWidgets.Data.Svg
import Vizagrams.Style
--import Vizagrams.VizMark
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

def vecToPoint (x : Float^[2]) (fr : Frame) : Point fr :=
  Point.abs x[0] x[1]

def pointToVec {fr : Frame} (p : Point fr) : Float^[2] :=
  match Point.toAbsolute p with
    | (x, y) => ⊞[x,y]

def geomToShape (g : Geom) (fr : Frame) : Shape fr :=
  match g with
  | Geom.line src trg => Shape.line (vecToPoint src fr) (vecToPoint trg fr)
  | Geom.circle r c => Shape.circle (vecToPoint c fr) (Size.abs r)
  | Geom.polyline points => Shape.polyline (points.map fun p => vecToPoint p fr)
  | Geom.polygon points => Shape.polygon (points.map fun p => vecToPoint p fr)

def primToElem (p : Prim) (fr : Frame) : Element fr :=
  { shape := geomToShape p.geom fr
  , fillColor := p.s.fillColor
  , strokeColor := p.s.strokeColor
  , strokeWidth := styleToSize p.s.strokeWidth fr
  }


def drawsvg (a : Array Prim) (fr : Frame := frame) : ProofWidgets.Html :=
  let svg : ProofWidgets.Svg fr := { elements := Array.map (λx => primToElem x fr) a}
  svg.toHtml

def draw (t : 𝕋 GraphicalMark.Mark) (fr : Frame := frame) : ProofWidgets.Html :=
  drawsvg (flat t ) fr

def NewCircle (r : Float) (c : Float^[2]) (st : Style := {fillColor := Color.mk 0 0 0}) : Prim :=
  let rc := Geom.circle r c
  {geom := rc, s := st}

def NewPolygon (pts : Array (Float^[2])) (st : Style := {fillColor := Color.mk 0 0 0}) : Prim :=
  let p := Geom.polygon pts
  {geom := p , s := st}

def NewLine ( l₁ l₂  : Float^[2] ) ( st : Style := {strokeColor := Color.mk 0 0 0} ): Prim :=
  let line := Geom.line l₁ l₂
  {geom := line , s := st}

def BoundingBox.toFrame (bb : BoundingBox) : Frame :=
  { frame with
    xmin  := bb.lower[0],
    ymin  := bb.lower[1],
    xSize := bb.upper[0] - bb.lower[0]
  }


end VizBackend
