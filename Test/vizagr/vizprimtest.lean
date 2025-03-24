import Vizagrams.VizPrim
import Vizagrams.VizBackend
import ProofWidgets.Component.HtmlDisplay

namespace testViz
open Sty GeometricPrimitive GeometricTransformation
open GraphicalPrimitive
open VizBackend
open ProofWidgets Svg

def line (src : Float^[2] := ⊞[0.0,0.0]) (tgt : Float^[2] := ⊞[1.0,1.0]) :=   Geom.line src tgt
def circle (r : Float := 1.0) (c : Float^[2] := ⊞[0.0,0.0]) := Geom.circle r c
def polyline (points : Array (Float^[2]) := #[⊞[-1.0,0.0],⊞[0.5,0.5], ⊞[1.0,0.0]]) := Geom.polyline points
def polygon (points : Array (Float^[2]) := #[⊞[-0.5,-0.5],⊞[0.5,-0.5], ⊞[0.5,0.5],⊞[-0.5,0.5]]) := Geom.polygon points

#check circle

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