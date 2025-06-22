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

def mod2π (θ : Float) : Float :=
  let twoPi := 2.0 * π
  let r := θ - twoPi * Float.floor (θ / twoPi)
  if r < 0 then r + twoPi else r

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
  | .arc rx ry c rot init final =>
    let p0 := rotateVec (pointOnEllipse init rx ry) rot + c
    let p1 := rotateVec (pointOnEllipse final rx ry) rot + c
    let flipY (y : Float) := 2 * fr.ymin + Frame.ySize fr - y
    let x0 := toString (p0 0)
    let y0 := toString (flipY (p0 1))
    let x1 := toString (p1 0)
    let y1 := toString (flipY (p1 1))
    let θdiff := mod2π (final - init)
    let largeArc := if Float.abs θdiff > π  then "1" else "0"
    let sweep    := if θdiff ≥ 0 then "0" else "1"
    let rotDeg   := toString (rot * 180.0 / π )

    let d := "M " ++ x0 ++ " " ++ y0 ++
            " A " ++ toString rx ++ " " ++ toString ry ++ " " ++ rotDeg ++
            " " ++ largeArc ++ " " ++ sweep ++ " " ++ x1 ++ " " ++ y1

    Shape.path d
  | .qbezier bpts cpts =>
      if h : bpts.size ≥ 2 ∧ cpts.size == bpts.size - 1 then
        let flipY (y : Float) := 2 * fr.ymin + Frame.ySize fr - y
        let start := vecToPoint bpts[0]! fr
        let (x0, y0) := start.toAbsolute
        let y0 := flipY y0
        let d :=
          List.range (bpts.size - 1) |>.map (fun i =>
            let p1 := vecToPoint (bpts[i+1]!) fr
            let c  := vecToPoint (cpts[i]!) fr
            let (x1, y1) := p1.toAbsolute
            let (cx, cy) := c.toAbsolute
            s!"Q {cx} {flipY cy}, {x1} {flipY y1}"
          ) |>.foldl (· ++ " " ++ ·) ("M " ++ toString x0 ++ " " ++ toString y0)
        Shape.path d
      else
        Shape.path ""
  | .cbezier bpts cpts =>
      if h : bpts.size ≥ 2 ∧ cpts.size == 2 * (bpts.size - 1) then
        let flipY (y : Float) := 2 * fr.ymin + Frame.ySize fr - y
        let start := vecToPoint bpts[0]! fr
        let (x0, y0) := start.toAbsolute
        let y0 := flipY y0
        let d :=
          List.range (bpts.size - 1) |>.map (fun i =>
            let p1 := vecToPoint (bpts[i+1]!) fr
            let c1 := vecToPoint (cpts[2 * i]!) fr
            let c2 := vecToPoint (cpts[2 * i + 1]!) fr
            let (x1, y1) := p1.toAbsolute
            let (x1, y1) := (x1, flipY y1)
            let (cx1, cy1) := c1.toAbsolute
            let (cx2, cy2) := c2.toAbsolute
            s!"C {cx1} {flipY cy1}, {cx2} {flipY cy2}, {x1} {y1}"
          ) |>.foldl (· ++ " " ++ ·) ("M " ++ toString x0 ++ " " ++ toString y0)
        Shape.path d
      else
        Shape.path ""


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

def NewCircle (r : Float := 1) (c : Vec2 := ![0,0]) (st : Style := {fillColor := Color.mk 0 0 0}) : Prim :=
  let rc := Geom.circle r c
  {geom := rc, style := st}

def NewPolygon (pts : Array (Vec2)) (st : Style := {fillColor := Color.mk 0 0 0}) : Prim :=
  let p := Geom.polygon pts
  {geom := p , style := st}

def NewLine ( l₁ l₂  : Vec2 ) ( st : Style := {strokeColor := Color.mk 0 0 0} ): Prim :=
  let line := Geom.line l₁ l₂
  {geom := line , style := st}

def NewText (content : String) (pos : Vec2 := ![0,0] ) (size : Float := 1) ( st : Style := {fillColor := Color.mk 0 0 0} ): Prim :=
  let text := Geom.text pos content size
  {geom := text , style := st}

def NewQBezier (bpts cpts : Array Vec2  ) (st : Style := {}) : Prim :=
  let bezier := Geom.qbezier bpts cpts
  {geom := bezier , style := st}

def BoundingBox.toFrame (bb : Envelope.BoundingBox) : Frame :=
  let dx := (bb.upper 0) - (bb.lower 0)
  let dy := (bb.upper 1) - (bb.lower 1)
  let aspect := frame.height.toFloat / frame.width.toFloat
  let xSize' := max dx (dy / aspect)
  { frame with
    xmin  := bb.lower 0,
    ymin  := bb.lower 1,
    xSize := xSize' }

def draw₁ (t : 𝕋 GraphicalMark.Mark) : ProofWidgets.Html :=
  draw t (BoundingBox.toFrame (Envelope.boundingBox𝕋 t))

end VizBackend
