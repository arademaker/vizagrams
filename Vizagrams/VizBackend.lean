/-
Copyright (c) 2025 Henrique Borges. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Davi Barreira, Henrique Borges
-/
import ProofWidgets.Data.Svg
import Vizagrams.Style
import Vizagrams.Envelope
import Vizagrams.FreeMonad

/-!
# Visualization Backend for Vizagrams

This module provides the rendering backend that transforms Vizagrams' abstract graphical
representations into concrete SVG output. It serves as the bridge between the mathematical
representation of diagrams and their visual presentation.

## Architecture

The rendering pipeline follows this flow:

```
𝕋 Mark  →  Array Prim  →  Array (Element Frame)  →  SVG  →  HTML
   ↓           ↓                ↓                    ↓        ↓
 (flat)   (primToElem)    (Svg.toHtml)         (render)  (display)
```

## Key Components

1. **Coordinate Conversion**: Transform between `Vec2` (abstract coordinates) and
   `Point Frame` (SVG coordinates)

2. **Geometry Rendering**: Convert `Geom` primitives to SVG `Shape` elements

3. **Frame Management**: Handle viewport and coordinate system transformations

4. **Helper Constructors**: Convenient functions like `NewCircle`, `NewPolygon` for
   creating common primitives

## Drawing Functions

- `draw`: Render a diagram tree with a custom frame
- `draw₁`: Automatically compute optimal frame from bounding box
- `drawsvg`: Render an array of primitives directly

## Usage Example

```lean
-- Create marks
let circle := NewCircle 1 ![0, 0] {fillColor := Color.mk 1 0 0}
let square := NewPolygon #[![-1,-1], ![1,-1], ![1,1], ![-1,1]] {}

-- Compose and render
let diagram := circle + square
#html draw₁ diagram  -- Automatically fits to content
```
-/

open ProofWidgets Svg
open GeometricPrimitive
open GraphicalPrimitive
open Sty
open FreeMonad
open LinearAlgebra

namespace VizBackend

/--
Default frame for rendering diagrams.
Defines a 400x400 pixel viewport with coordinate system origin at (0, 0)
and a default x-axis range of 10 units.
-/
private def frame : Frame where
  xmin   := 0
  ymin   := 0
  xSize  := 10
  width  := 400
  height := 400

def defaultFrame : Frame where
  xmin   := -3
  ymin   := -3
  xSize  := 10
  width  := 400
  height := 400

/--
Convert a `Vec2` (abstract 2D vector) to a `Point` in the given frame's coordinate system.
-/
def vecToPoint (x : Vec2) (fr : Frame) : Point fr :=
  Point.abs (x 0) (x 1)

/--
Convert a `Point` in a frame's coordinate system back to a `Vec2`.
-/
def pointToVec {fr : Frame} (p : Point fr) : Vec2 :=
  match Point.toAbsolute p with
    | (x, y) => ![x,y]

/--
Normalize an angle to the range [0, 2π).
This is used for arc rendering to ensure angles are in the correct range.
-/
def mod2π (θ : Float) : Float :=
  let twoPi := 2.0 * π
  let r := θ - twoPi * Float.floor (θ / twoPi)
  if r < 0 then r + twoPi else r

/--
Convert a `Vec2` to a space-separated coordinate string for SVG path data.
-/
def getCoordinates (v : Vec2) : String :=
  s!"{v 0} {v 1}"

/--
**geomToShape**: Convert a geometric primitive to an SVG shape.

This is the core rendering function that maps our abstract geometry representation
to ProofWidgets' SVG shapes. It handles:
- Basic shapes: lines, circles, ellipses, rectangles
- Paths: polylines, polygons, custom path data
- Text rendering
- Complex curves: arcs, quadratic and cubic Bézier curves

**Special handling:**
- Arcs are converted to SVG arc path commands with proper angle normalization
- Bézier curves use SVG path syntax (Q for quadratic, C for cubic)
- Y-coordinates are flipped to match SVG's top-down coordinate system
-/
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
    let p0 := rotateVec2 (pointOnEllipse init rx ry) rot + c
    let p1 := rotateVec2 (pointOnEllipse final rx ry) rot + c
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

  | .qbezier m q =>
    let d := s!"M {getCoordinates m } Q {getCoordinates q.fst} {getCoordinates q.snd}"
    Shape.path d

  | .cbezier m (c1, c2, p1) =>
        let flipY (y : Float) := 2 * fr.ymin + Frame.ySize fr - y
        let start := vecToPoint m fr
        let endPt := vecToPoint p1 fr
        let ctrl1 := vecToPoint c1 fr
        let ctrl2 := vecToPoint c2 fr
        let (x0, y0) := start.toAbsolute
        let (x1, y1) := endPt.toAbsolute
        let (cx1, cy1) := ctrl1.toAbsolute
        let (cx2, cy2) := ctrl2.toAbsolute
        let d := s!"M {x0} {flipY y0} C {cx1} {flipY cy1}, {cx2} {flipY cy2}, {x1} {flipY y1}"
        Shape.path d

/--
Convert a `Prim` (primitive with styling) to an SVG `Element`.
Combines the geometric shape with its associated styling attributes.
-/
def primToElem (p : Prim) (fr : Frame) : Element fr :=
  { shape := geomToShape p.geom fr
  , fillColor := p.style.fillColor
  , strokeColor := p.style.strokeColor
  , strokeWidth := styleToSvgSize p.style.strokeWidth fr
  }

/--
Render an array of primitives as SVG within the given frame.
This is the low-level rendering function that directly converts primitives to HTML.
-/
def drawsvg (a : Array Prim) (fr : Frame := frame) : ProofWidgets.Html :=
  let svg : ProofWidgets.Svg fr := { elements := Array.map (λx => primToElem x fr) a}
  svg.toHtml

/--
**draw**: Render a diagram tree using a specified frame.

This function:
1. Evaluates the tree `t : 𝕋 Mark` to an array of primitives (via `flat`)
2. Converts primitives to SVG elements
3. Returns HTML for display

**Parameters:**
- `t`: The diagram tree to render
- `fr`: The coordinate frame (defaults to standard 400x400 viewport)

**Usage:**
```lean
let customFrame : Frame := { xmin := -5, ymin := -5, xSize := 10, ... }
#html draw myDiagram customFrame
```
-/
def draw (t : 𝕋 GraphicalMark.Mark) (fr : Frame := frame) : ProofWidgets.Html :=
  drawsvg (flat t ) fr

/-!
### Primitive Constructors

Convenience functions for creating common geometric primitives with sensible defaults.
These provide an ergonomic API for building diagrams without manually constructing
`Prim` structures.
-/

/--
Create a circle primitive.

**Parameters:**
- `r`: Radius (default: 1)
- `c`: Center position as `Vec2` (default: origin)
- `st`: Style (default: black fill)
-/
def NewCircle (r : Float := 1) (c : Vec2 := ![0,0]) (st : Style := {fillColor := Color.mk 0 0 0}) : Prim :=
  let rc := Geom.circle r c
  {geom := rc, style := st}

/--
Create a polygon primitive from an array of vertices.

**Parameters:**
- `pts`: Array of vertices as `Vec2`
- `st`: Style (default: black fill)
-/
def NewPolygon (pts : Array (Vec2)) (st : Style := {fillColor := Color.mk 0 0 0}) : Prim :=
  let p := Geom.polygon pts
  {geom := p , style := st}

/--
Create a line segment primitive.

**Parameters:**
- `l₁`: Start point as `Vec2`
- `l₂`: End point as `Vec2`
- `st`: Style (default: black stroke)
-/
def NewLine ( l₁ l₂  : Vec2 ) ( st : Style := {strokeColor := Color.mk 0 0 0} ): Prim :=
  let line := Geom.line l₁ l₂
  {geom := line , style := st}

/--
Create a text primitive.

**Parameters:**
- `content`: The text string to display
- `pos`: Position as `Vec2` (default: origin)
- `size`: Font size (default: 1)
- `st`: Style (default: black fill)
-/
def NewText (content : String) (pos : Vec2 := ![0,0] ) (size : Float := 1) ( st : Style := {fillColor := Color.mk 0 0 0} ): Prim :=
  let text := Geom.text pos content size
  {geom := text , style := st}

/--
Create a quadratic Bézier curve primitive.

**Parameters:**
- `m`: Start point (move-to position)
- `q`: Control point and end point as `Vec2 × Vec2`
- `st`: Style (default: empty)
-/
def NewQBezier (m : Vec2) (q : Vec2 × Vec2) (st : Style := {}) : Prim :=
  let bezier := Geom.qbezier m q
  {geom := bezier , style := st}

/--
Convert a bounding box to a frame that tightly fits the content.

This automatically computes the viewport dimensions and position to optimally
display the given bounding box while preserving the aspect ratio.

**Parameters:**
- `bb`: The bounding box computed from diagram content

**Returns:** A frame that fits the bounding box with appropriate margins
-/
def BoundingBox.toFrame (bb : Envelope.BoundingBox) : Frame :=
  let dx := (bb.upper 0) - (bb.lower 0)
  let dy := (bb.upper 1) - (bb.lower 1)
  let aspect := frame.height.toFloat / frame.width.toFloat
  let xSize' := max dx (dy / aspect)
  { frame with
    xmin  := bb.lower 0,
    ymin  := bb.lower 1,
    xSize := xSize' }

/--
**draw₁**: Render a diagram with automatically computed optimal frame.

This is the recommended rendering function for most use cases. It:
1. Computes the bounding box of the diagram
2. Creates a frame that tightly fits the content
3. Renders the diagram in this optimal viewport

**Usage:**
```lean
#html draw₁ myDiagram  -- Automatic frame fitting
```

This ensures the entire diagram is visible and well-centered, regardless of its size or position.
-/
def draw₁ (t : 𝕋 GraphicalMark.Mark) : ProofWidgets.Html :=
  draw t (BoundingBox.toFrame (Envelope.boundingBox𝕋 t))

end VizBackend
