/-
Copyright (c) 2025 Henrique Borges. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Davi Barreira, Henrique Borges
-/
import Vizagrams.Geom
import Vizagrams.Style

/-!
# Graphical Primitives for Vizagrams

This module defines the core graphical primitive type `Prim`, which combines geometric shapes
with styling attributes. It provides:
- `Prim`: A geometric shape with associated styling (fill color, stroke color, etc.)
- Transformation instances for applying geometric transformations and style modifications
- Composition operations for combining multiple primitives

## Key Components

- **Prim Structure**: Combines a `Geom` (geometric shape) with a `Style` (visual attributes)
- **Geometric Transformations**: Apply `Mat2Vec2` transformations to primitives and arrays
- **Style Transformations**: Apply `Style` modifications to primitives and arrays
- **Composition Operations**: Combine primitives using the `⊕` operator
-/

namespace GraphicalPrimitive

open GeometricPrimitive
open Sty
open LinearAlgebra

/--
A graphical primitive combining geometric shape with visual styling.
This is the fundamental drawing unit in Vizagrams.
-/
structure Prim where
  /-- The geometric shape (circle, line, polygon, etc.) -/
  geom : Geom
  /-- The visual styling (colors, stroke width, etc.) -/
  style : Style
deriving Repr

/-- A single primitive can be treated as an array containing just that primitive. -/
instance : Coe Prim (Array Prim) where
  coe p := #[p]

/-- A primitive can be treated as its underlying geometry. -/
instance : Coe Prim Geom where
  coe p := p.geom

/--
Apply a geometric transformation to a primitive.
The transformation affects only the geometry, leaving the style unchanged.
-/
instance : HMul Mat2Vec2 Prim Prim where
  hMul g p := { p with geom := g * p.geom }

/--
Apply a geometric transformation to an array of primitives.
Each primitive's geometry is transformed independently.
-/
instance : HMul Mat2Vec2 (Array Prim) (Array Prim) where
  hMul g ps := ps.map (g * ·)

/--
Apply a style modification to a primitive.
The new style is composed with the existing style using the `Append` instance,
where the new style takes precedence for conflicting attributes.
-/
instance : HMul Style Prim Prim where
  hMul s p := { p with style := s ++ p.style }

/--
Apply a style modification to an array of primitives.
Each primitive's style is modified independently.
-/
instance : HMul Style (Array Prim) (Array Prim) where
  hMul s ps := ps.map (s * ·)

/-- Apply a style transformation to a single primitive. -/
def applyStyle (s : Style) (p : Prim) : Prim := s * p

/-- Apply a style transformation to an array of primitives. -/
def applyStyleArray (s : Style) (ps : Array Prim) : Array Prim := s * ps

/-- Apply a geometric transformation to a single primitive. -/
def applyTransform (g : Mat2Vec2) (p : Prim) : Prim := g * p

/-- Apply a geometric transformation to an array of primitives. -/
def applyTransformArray (g : Mat2Vec2) (ps : Array Prim) : Array Prim := g * ps

/--
Type class for composing graphical primitives into arrays.
This enables the `⊕` operator for combining drawing elements.
-/
class Compose (α : Type u) (β : Type v) where
  /-- Compose two graphical elements into an array of primitives. -/
  compose : α → β → Array Prim

/-- Compose two individual primitives. -/
instance : Compose Prim Prim where
  compose p₁ p₂ := #[p₁, p₂]

/-- Compose a primitive with an array of primitives. -/
instance : Compose Prim (Array Prim) where
  compose p ps := #[p] ++ ps

/-- Compose an array of primitives with a single primitive. -/
instance : Compose (Array Prim) Prim where
  compose ps p := ps ++ #[p]

/-- Compose two arrays of primitives. -/
instance : Compose (Array Prim) (Array Prim) where
  compose ps₁ ps₂ := ps₁ ++ ps₂

/--
Composition operator for combining graphical primitives.
This is the primary way to build complex drawings from simple parts.

## Examples
```lean
let circle := Prim.mk (Geom.circle 1.0 ![0,0]) defaultStyle
let square := Prim.mk (Geom.rect ![1,1] 2.0 2.0) defaultStyle
let combined := circle ⊕ square
```
-/
infixr:80 " ⊕ " => Compose.compose

/-- Create a primitive with default styling. -/
def mk (geom : Geom) (style : Style := {}) : Prim :=
  ⟨geom, style⟩

/-- Create a circle primitive with specified radius, center, and optional style. -/
def circle (r : Float) (c : Vec2 := ![0,0]) (style : Style := {}) : Prim :=
  mk (Geom.circle r c) style

/-- Create a line primitive with specified endpoints and optional style. -/
def line (src trg : Vec2) (style : Style := {}) : Prim :=
  mk (Geom.line src trg) style

/-- Create a rectangle primitive with specified corner, dimensions, and optional style. -/
def rect (corner : Vec2) (width height : Float) (style : Style := {}) : Prim :=
  mk (Geom.rect corner width height) style

/-- Create a polygon primitive with specified vertices and optional style. -/
def polygon (points : Array Vec2) (style : Style := {}) : Prim :=
  mk (Geom.polygon points) style

/-- Create a text primitive with specified position, content, size, and optional style. -/
def text (pos : Vec2) (content : String) (size : Float := 1.0) (style : Style := {}) : Prim :=
  mk (Geom.text pos content size) style

end GraphicalPrimitive
