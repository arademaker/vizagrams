import SciLean
import Vizagrams.Style
import Vizagrams.Geom
import Vizagrams.Transformations

import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay

set_option autoImplicit true
set_default_scalar Float

open Sty GeometricPrimitive GeometricTransformation
open ProofWidgets Svg

namespace GraphicalPrimitive

private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400

structure Prim where
  geom : Geom
  s := ({} : Style) -- Uses the default Style none none none
deriving Repr

structure H where
  g : G
  s : Style
instance : Repr H where
    reprPrec h _ := "{ g:= G, s := " ++ repr h.s ++ " }"

instance : HMul G Prim Prim where
  hMul g p := {p with geom := g * p.geom}

instance : HMul Style Prim Prim where
  hMul s p := {p with s := Style.comp s  p.s}

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
def line (src : Float^[2] := ⊞[0.0,0.0]) (tgt : Float^[2] := ⊞[1.0,1.0]) :=   Geom.line src tgt
def circle (r : Float := 1.0) (c : Float^[2] := ⊞[0.0,0.0]) := Geom.circle r c
def polyline (points : Array (Float^[2]) := #[⊞[-1.0,0.0],⊞[0.5,0.5], ⊞[1.0,0.0]]) := Geom.polyline points
def polygon (points : Array (Float^[2]) := #[⊞[-0.5,-0.5],⊞[0.5,-0.5], ⊞[0.5,0.5],⊞[-0.5,0.5]]) := Geom.polygon points

private def x : Prim := {geom := circle, s := {fillColor := Color.mk 1 1 0}}

private def y : Prim := {geom := line,  s := {strokeColor := Color.mk 1 0 0}}
private def z : Prim := {geom := circle 0.5,  s := {fillColor := Color.mk 0 1 1}}

private def s : Style := {strokeColor := Color.mk 1.0 0.0 0.0, strokeWidth := some (StyleSize.px 4)}

#html drawsvg #[x]

#html drawsvg #[x,y,(G.translate ⊞[1,1]) * z]

#html drawsvg #[x, (G.scale 1)*y, (G.translate ⊞[1,1]) * z]
#html drawsvg #[s*x, y, (G.translate ⊞[1,1]) * z]

#eval polygon

private def t : Prim := {geom := polygon , s}
#html drawsvg #[t]
private def r : Prim := {geom := polyline , s}
#html drawsvg #[r]

end GraphicalPrimitive
