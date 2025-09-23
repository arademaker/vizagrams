/-
Copyright (c) 2025 Henrique Borges. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Davi Barreira, Henrique Borges
-/
import Vizagrams.FreeMonad

/-!
# Envelope Functions and Bounding Box Operations for Vizagrams

This module implements envelope functions and bounding box calculations for geometric primitives
and marks in the Vizagrams system. The envelope function is fundamental to the layout system,
enabling precise positioning and alignment of graphical elements.

## Core Concepts

### Envelope Function
The envelope function `envelope(g, v)` computes the maximum extent of a geometric shape `g`
in the direction of vector `v`. This is mathematically defined as:
```
envelope(g, v) = max{⟨p, v⟩ | p ∈ g}
```
where `⟨p, v⟩` is the dot product and the maximum is taken over all points `p` in the shape.

### Bounding Boxes
Bounding boxes provide axis-aligned rectangular bounds for shapes, computed using envelope
functions in the four cardinal directions. They enable efficient spatial reasoning and
layout operations.

### Layout Operations
The module provides high-level layout functions like `hStack`, `vStack`, and directional
positioning that use envelope calculations to arrange elements with precise spacing.

## Key Components

- **Envelope Functions**: Core `envelope` function for `Geom`, `Prim`, and `Mark` types
- **Bounding Box Types**: `BoundingBox` structure with union operations
- **Layout Operators**: Infix operators for intuitive spatial composition (`→`, `←`, `↑`, `↓`)
- **Positioning Functions**: Gap-aware positioning using envelope calculations
- **Centering Operations**: Functions to center geometric elements

## Mathematical Foundation

The envelope function provides the mathematical foundation for:
1. **Collision Detection**: Non-overlapping placement of elements
2. **Alignment**: Precise edge-to-edge alignment
3. **Distribution**: Even spacing of multiple elements
4. **Composition**: Building complex layouts from simple parts

## Layout Algebra

The layout operations form an algebraic structure where:
- Layout operators are associative: `(a → b) → c = a → (b → c)`
- Gap parameters provide fine control over spacing
- Direction vectors enable arbitrary positioning directions
- The system preserves geometric relationships under transformations

-/

namespace Envelope

open LinearAlgebra
open GeometricPrimitive
open GraphicalPrimitive
open GraphicalMark
open FreeMonad

/-!
### Utility Functions for Envelope Calculations

Fundamental operations for computing envelopes and sampling curves.
-/

/--
**Envelope of Point Array**: Calculate envelope of an array of points.

Given a direction vector `v` and an array of points `pts`, computes the maximum
dot product `⟨p, v⟩` over all points `p` in the array.

**Parameters:**
- `pts`: Array of 2D points
- `v`: Direction vector (should be normalized)

**Returns:** Maximum projection in direction `v`, or 0.0 for empty arrays
-/
def envelopePts (pts : Array Vec2) (v : Vec2) : Float :=
  if h : pts.size > 0 then
    let first := pts[0]!
    let init  := ⟪v, first⟫
    pts.foldl (fun acc p => max acc ⟪v, p⟫) init
  else
    0.0

/--
**Quadratic Bezier Sampling**: Sample points along a quadratic Bezier curve.

Generates `n` evenly spaced points along the quadratic Bezier curve defined by
control points `p0` (start), `c` (control), and `p1` (end).

**Mathematical Formula:**
```
B(t) = (1-t)² · p0 + 2(1-t)t · c + t² · p1
```
where `t ∈ [0,1]` parameterizes the curve.
-/
def sampleQBezier (p0 c p1 : Vec2) (n : Nat) : Array Vec2 :=
  List.range n |>.map (fun i =>
    let t := Float.ofNat i / Float.ofNat (n - 1)
    let oneMinusT := 1 - t
    (oneMinusT ^ 2) • p0 +
    (2 * oneMinusT * t) • c +
    (t ^ 2) • p1
  ) |>.toArray

/--
**Cubic Bezier Sampling**: Sample points along a cubic Bezier curve.

Generates `n` evenly spaced points along the cubic Bezier curve defined by
control points `p0` (start), `c1`, `c2` (control points), and `p1` (end).

**Mathematical Formula:**
```
B(t) = (1-t)³ · p0 + 3(1-t)²t · c1 + 3(1-t)t² · c2 + t³ · p1
```
where `t ∈ [0,1]` parameterizes the curve.
-/
def sampleCBezier (p0 c1 c2 p1 : Vec2) (n : Nat) : Array Vec2 :=
  List.range n |>.map (fun i =>
    let t := Float.ofNat i / Float.ofNat (n - 1)
    let oneMinusT := 1 - t
    (oneMinusT ^ 3) • p0 +
    (3 * oneMinusT ^ 2 * t) • c1 +
    (3 * oneMinusT * t ^ 2) • c2 +
    (t ^ 3) • p1
  ) |>.toArray

/-!
### Core Envelope Function

The fundamental envelope function for geometric primitives.
-/

/--
**Geometric Envelope Function**: Compute envelope of any geometric primitive.

This is the core function that computes the envelope (maximum extent) of a geometric
shape in a given direction. The direction vector is automatically normalized.

**Parameters:**
- `g`: Geometric primitive of any type
- `v'`: Direction vector (will be normalized internally)

**Returns:** Maximum projection of the shape in the given direction

**Implementation Notes:**
- For exact shapes (circle, rectangle), uses analytical formulas
- For complex curves (Bezier, arcs), uses sampling-based approximation
- Returns 0.0 for undefined or degenerate cases
-/
def envelope (g : Geom) (v' : Vec2) : Float :=
  let v := normalize v'
  match g with
  | .circle r c =>
      ⟪v, c + r • v⟫
  | .line p q =>
      max ⟪v, p⟫ ⟪v, q⟫
  | .polyline pts =>
      pts.foldl (fun acc p => max acc ⟪v, p⟫) ⟪v, pts[0]!⟫
  | .polygon pts =>
      pts.foldl (fun acc p => max acc ⟪v, p⟫) ⟪v, pts[0]!⟫
  | .ellipse rx ry c =>
      -- Proper ellipse envelope calculation
      -- For ellipse with semi-axes rx, ry, envelope in direction v is:
      -- ⟪v, center⟫ + √((rx * v_x)² + (ry * v_y)²)
      let vx := v 0
      let vy := v 1
      let radius := Float.sqrt ((rx * vx) ^ 2 + (ry * vy) ^ 2)
      ⟪v, c⟫ + radius
  | .rect corner width height =>
      let p1 := corner
      let p2 := ![corner 0 + width, corner 1]
      let p3 := ![corner 0, corner 1 + height]
      let p4 := ![corner 0 + width, corner 1 + height]
      #[p1, p2, p3, p4].foldl (fun acc p => max acc ⟪v, p⟫) ⟪v, p1⟫
  | .path _ =>
      -- No semantics defined, conservatively treat as zero envelope
      0.0
  | .text pos content size =>
      -- Proper text bounding box calculation
      -- Approximate character width as 0.6 * font size
      -- Font height as font size
      -- Assume baseline position (pos is bottom-left of text)
      let charWidth := 0.6 * size
      let textWidth := Float.ofNat content.length * charWidth
      let textHeight := size

      -- Text bounding box corners (bottom-left at pos)
      let bottomLeft := pos
      let bottomRight := ![pos 0 + textWidth, pos 1]
      let topLeft := ![pos 0, pos 1 + textHeight]
      let topRight := ![pos 0 + textWidth, pos 1 + textHeight]

      -- Find maximum projection of corners in direction v
      let corners := #[bottomLeft, bottomRight, topLeft, topRight]
      envelopePts corners v
  | .arc rx ry c rot init final =>
      -- Sample points along the arc and compute maximum envelope
      let steps := 32
      let angles := List.range steps |>.map (fun i =>
        let t := Float.ofNat i / Float.ofNat (steps : Nat)
        init + t * (final - init))
      let pts := angles.map (fun θ =>
        let p := pointOnEllipse θ rx ry
        rotateVec2 p rot + c)
      envelopePts pts.toArray v
  | .qbezier m (c, q) =>
      let pts := sampleQBezier m c q 20
      envelopePts pts v

  | .cbezier m (c1, c2, p1) =>
      let pts := sampleCBezier m c1 c2 p1 20
      envelopePts pts v


/-!
### Bounding Box Operations

Axis-aligned bounding boxes for efficient spatial reasoning.
-/

/--
**Bounding Box Structure**: Axis-aligned rectangular bounds.

Represents the minimal axis-aligned rectangle that contains a geometric shape.
Defined by lower-left (`lower`) and upper-right (`upper`) corner points.

**Invariant:** `lower.x ≤ upper.x` and `lower.y ≤ upper.y`
-/
structure BoundingBox where
  /-- Lower-left corner of the bounding box -/
  lower : Vec2
  /-- Upper-right corner of the bounding box -/
  upper : Vec2
deriving Repr, Inhabited

def boundingBox (g : Geom) : BoundingBox :=
  let ex₁ := envelope g (![1.0, 0.0])
  let ex₂ := envelope g (![-1.0, 0.0])
  let ey₁ := envelope g (![0.0, 1.0])
  let ey₂ := envelope g (![0.0, -1.0])
  { lower := ![-ex₂, -ey₂]
  , upper := ![ ex₁,  ey₁] }

/--
**Bounding Box Union**: Compute union of two bounding boxes.

Returns the smallest bounding box that contains both input boxes.
This operation is associative and commutative.
-/
def BoundingBox.union (b₁ b₂ : BoundingBox) : BoundingBox :=
  let i0 : Fin 2 := Fin.mk 0 (by decide)
  let i1 : Fin 2 := Fin.mk 1 (by decide)
  let lx := min (b₁.lower i0) (b₂.lower i0)
  let ly := min (b₁.lower i1) (b₂.lower i1)
  let ux := max (b₁.upper i0) (b₂.upper i0)
  let uy := max (b₁.upper i1) (b₂.upper i1)
  { lower := ![lx, ly], upper := ![ux, uy] }


def boundingBoxGroup (gs : Array Geom) : BoundingBox :=
  if h : gs.size > 0 then
    let firstBB := boundingBox gs[0]     -- bounding box of a single Geom
    gs.foldl (fun acc g => acc.union (boundingBox g)) firstBB
  else
    { lower := ![0.0, 0.0], upper := ![0.0, 0.0] }

def envelopePosition (g₁ : Geom) ( v : Vec2 ) ( g₂ : Geom ) (gap : Float := 0): Geom :=
  let v₁ := normalize v
  let offset := (envelope g₁ v₁) + (envelope g₂ (-v₁)) + gap
  let position := offset • v₁
  (translate position) * g₂

/-!
### Layout and Positioning Functions

High-level layout operations using envelope calculations.
-/

/-- **Horizontal Stack Right**: Position `g₂` to the right of `g₁`. -/
def hStackRight (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ![1,0] g₂

/-- **Horizontal Stack Left**: Position `g₂` to the left of `g₁`. -/
def hStackLeft (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ![-1,0] g₂

/-- **Vertical Stack Up**: Position `g₂` above `g₁`. -/
def vStackUp (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ![0,1] g₂

/-- **Vertical Stack Down**: Position `g₂` below `g₁`. -/
def vStackDown (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ![0,-1] g₂

def centerGeom (g : Geom) : Geom :=
  let dx := (envelope g (![1,0]) - envelope g (![-1,0])) / 2
  let dy := (envelope g (![0,1]) - envelope g (![0,-1])) / 2
  translate (![-dx, -dy]) * g


def boundingBoxPrim (p : Prim) : BoundingBox :=
  boundingBox p.geom

/-!
### Primitive-Level Bounding Box Operations

Bounding box calculations for graphical primitives.
-/

/-- **Primitive Bounding Box Union**: Union of bounding boxes from an array of primitives. -/
def boundingBoxPrims (ps : Array Prim) : BoundingBox :=
  if h : ps.size > 0 then
    let firstBB := boundingBoxPrim ps[0]
    ps.foldl (fun acc p => acc.union (boundingBoxPrim p)) firstBB
  else
    -- Empty array: degenerate bounding box at origin
    { lower := ![0.0,0.0], upper := ![0.0,0.0] }

def centerPrim (p : Prim) : Prim :=
  { geom := centerGeom p.geom, style := p.style }

def centerPrims (ps : Array Prim) : Array Prim :=
  ps.map centerPrim

def envelopePositionPrim (p₁ : Prim) (v : Vec2) (p₂ : Prim) (gap : Float := 0): Prim :=
  let g := envelopePosition p₁.geom v p₂.geom gap
  { geom := g , style := p₂.style : Prim}

def envelopeArray (A : Array Prim) (v : Vec2) : Float :=
  A.foldl (λ acc p => max acc (envelope p.geom v)) 0

def envelopePositionPrims (A : Array Prim) (v : Vec2) (p₂ : Prim) (gap : Float := 0) : Prim :=
  let v₁ := normalize v
  let offset := (envelopeArray A v₁) + (envelope p₂.geom (-v₁)) + gap
  let position := offset • v₁
  let g := translate position * p₂.geom
  { geom := g , style := p₂.style : Prim }

def envelopePositionPrimsArray (A : Array Prim) (v : Vec2) (B : Array Prim) (gap : Float := 0) : Array Prim :=
  let v₁ := normalize v
  let offset := (envelopeArray A v₁) + (envelopeArray B (-v₁)) + gap
  let position := offset • v₁
  B.map (fun p =>
    { geom := translate position * p.geom
      style := p.style : Prim })

/-!
### Array-Level Layout Operations

Layout operations for arrays of primitives with gap control.
-/

/-- **Horizontal Stack Right (Arrays)**: Position array `p₂` to the right of array `A`. -/
def hStackRightPrims (A : Array Prim) (p₂ : Array Prim) (gap : Float := 0) : Array Prim :=
  A ⊕ (envelopePositionPrimsArray A ![1,0] p₂ gap)

/-- **Horizontal Stack Left (Arrays)**: Position array `p₂` to the left of array `A`. -/
def hStackLeftPrims (A : Array Prim) (p₂ : Array Prim) (gap : Float := 0) : Array Prim :=
  A ⊕ (envelopePositionPrimsArray A ![-1,0] p₂ gap)

/-- **Vertical Stack Up (Arrays)**: Position array `p₂` above array `A`. -/
def vStackUpPrims (A : Array Prim) (p₂ : Array Prim) (gap : Float := 0) : Array Prim :=
  A ⊕ (envelopePositionPrimsArray A ![0,1] p₂ gap)

/-- **Vertical Stack Down (Arrays)**: Position array `p₂` below array `A`. -/
def vStackDownPrims (A : Array Prim) (p₂ : Array Prim) (gap : Float := 0) : Array Prim :=
  A ⊕ (envelopePositionPrimsArray A ![0,-1] p₂ gap)

infixr:70 " → " => hStackRightPrims
infixr:70 " ← " => hStackLeftPrims
infixr:70 " ↑ " => vStackUpPrims
infixr:70 " ↓ " => vStackDownPrims

-- Layout operators with explicit gap control: "A →[g] B" syntax
notation:70 A " →[" g "] " B => hStackRightPrims A B g
notation:70 A " ←[" g "] " B => hStackLeftPrims  A B g
notation:70 A " ↑[" g "] " B => vStackUpPrims    A B g
notation:70 A " ↓[" g "] " B => vStackDownPrims  A B g

/-!
### Mark-Level Bounding Box Operations

Bounding box calculations for graphical marks using their rendering interface.
-/

/--
**Mark Bounding Box (Generic)**: Bounding box of a mark via its θ interface.

Computes bounding box by aggregating primitives returned by the mark's θ function.
Works for any type implementing `MarkInterface`.
-/
def boundingBoxMark {T : Type} [inst : MarkInterface T] (m : T) : BoundingBox :=
  boundingBoxPrims (inst.θ m)

def boundingBoxOfMark (m : Mark) : BoundingBox :=
  boundingBoxPrims m.θ


def boundingBox𝕋 (t : 𝕋 Mark) : BoundingBox :=
  boundingBoxPrims (flat t)

def envelopePositionMarks (𝕄₁ : 𝕋 Mark) ( v : Vec2) (𝕄₂ : 𝕋 Mark) (gap : Float := 0): 𝕋 Mark :=
  let 𝕞₁ := flat 𝕄₁
  let 𝕞₂ := flat 𝕄₂
  let v₁ := normalize v
  let offset := (envelopeArray 𝕞₁ v₁) + (envelopeArray 𝕞₂ (-v₁)) + gap
  let position := offset • v₁
  let h : ℍ := { s := {} , g := translate position }
  h * 𝕄₂

/-!
### Mark-Level Layout Operations

High-level layout operations for the free monad mark system.
-/

/-- **Horizontal Stack Right (Marks)**: Position mark tree `𝕄₂` to the right of `𝕄₁`. -/
def hStackRightMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ![1,0] 𝕄₂ gap)

/-- **Horizontal Stack Left (Marks)**: Position mark tree `𝕄₂` to the left of `𝕄₁`. -/
def hStackLeftMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ![-1,0] 𝕄₂ gap)

def vStackUpMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ![0,1] 𝕄₂ gap)

def vStackDownMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ![0,-1] 𝕄₂ gap)

infixr:70 " → " => hStackRightMarks
infixr:70 " ← " => hStackLeftMarks
infixr:70 " ↑ " => vStackUpMarks
infixr:70 " ↓ " => vStackDownMarks

notation:70 A " →[" g "] " B => hStackRightMarks A B g
notation:70 A " ←[" g "] " B => hStackLeftMarks  A B g
notation:70 A " ↑[" g "] " B => vStackUpMarks    A B g
notation:70 A " ↓[" g "] " B => vStackDownMarks  A B g
notation:70 A " ↗[" v ", " g "]" B => A + envelopePositionMarks A v B g

end Envelope
