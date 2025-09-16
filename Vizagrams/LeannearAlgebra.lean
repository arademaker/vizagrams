/-
Copyright (c) 2025 Henrique Borges. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrique Borges
-/
import Mathlib.Analysis.InnerProductSpace.Basic
import Mathlib.Data.Matrix.Auto
import Mathlib.Data.Matrix.Notation
import Mathlib.Analysis.Normed.Group.Basic
import Mathlib.LinearAlgebra.AffineSpace.AffineMap
import Mathlib.Data.Matrix.Basic
import Mathlib.Data.Matrix.Notation

/-!
# Linear Algebra Foundations for Vizagrams

This module establishes the mathematical foundation for 2D geometric operations in Vizagrams.
It provides vector spaces, affine transformations, and geometric operations that form the
basis for the categorical framework described in "Data Visualization from a Category Theory Perspective".

## Geometric Framework

Following Felix Klein's Erlanger Program, a geometry is determined by its symmetry group.
For Vizagrams, we work in the 2D Euclidean plane with the group of affine transformations,
which includes:
- Translations
- Rotations
- Uniform scaling
- Reflections

## Key Components

- **Vec2**: 2D vectors over Float (the geometric space)
- **Mat2**: 2×2 matrices for linear transformations
- **Mat2Vec2**: Affine transformations (linear + translation)
- **apply/compose**: Operations for categorical transformation structure

## Categorical Perspective

In the categorical framework:
- Vec2 forms the object space for our geometry
- Mat2Vec2 provides the morphisms (transformations)
- `apply` implements morphism evaluation
- `compose` follows categorical laws (associativity, identity)
- This enables the geometric primitive system to be mathematically rigorous

-/

namespace LinearAlgebra

open Matrix Fin

/-!
### Constants and Basic Definitions

Mathematical constants and fundamental vector definitions.
-/

/-- Mathematical constant π with sufficient precision for geometric operations. -/
def π : Float := 3.141592653589793

/-- 2D vector type: functions from Fin 2 to Float. -/
abbrev Vec2 := Fin 2 → Float

/-- Standard basis vector e₁ = (1, 0). -/
def e₁ : Vec2 := ![1, 0]

/-- Standard basis vector e₂ = (0, 1). -/
def e₂ : Vec2 := ![0, 1]

/-- Zero vector (0, 0). -/
def nullVec2 : Vec2 := ![0.0, 0.0]

/-!
### Inner Product and Norms

Inner product structure making Vec2 into a Euclidean space.
This provides the geometric foundation for distances, angles, and orthogonality.
-/

/--
Inner product on Vec2 giving it Euclidean structure.
Essential for geometric operations like distance and angle calculations.
-/
instance : Inner Float Vec2 where
  inner v₁ v₂ := v₁ 0 * v₂ 0 + v₁ 1 * v₂ 1

/-- Convenient notation for inner products. -/
notation "⟪" x "," y "⟫" => inner Float x y

/-- Squared Euclidean norm (avoids square root for efficiency). -/
def normSquare (v : Vec2) : Float := ⟪v, v⟫

/-- Notation for squared norm. -/
notation:max "‖" v:max "‖²" => normSquare v

/-- Euclidean norm (length of vector). -/
def vecNorm (v : Vec2) : Float := Float.sqrt ⟪v, v⟫

/-- Standard notation for norm. -/
notation:max "‖" v:max "‖" => vecNorm v

/-!
### Vector Operations

Fundamental geometric operations on vectors.
-/

/--
Normalize a vector to unit length.
Returns the original vector if it's the zero vector to avoid division by zero.
-/
def normalize (v : Vec2) : Vec2 :=
  let n := vecNorm v
  if n == 0 then v else fun i => v i / n

/-- Euclidean distance between two points. -/
def distance (v₁ v₂ : Vec2) : Float := vecNorm (v₁ - v₂)

/-- Convenient notation for distance. -/
notation "d(" x "," y ")" => distance x y

/-- Angle between two vectors (in radians). -/
def angleBetween (v₁ v₂ : Vec2) : Float :=
  Float.acos (⟪v₁, v₂⟫ / (vecNorm v₁ * vecNorm v₂))

/-- Orthogonal projection of v₁ onto v₂. -/
def projection (v₁ v₂ : Vec2) : Vec2 :=
  (⟪v₁, v₂⟫ / normSquare v₂) • v₂

/-- 90-degree counterclockwise rotation: (x, y) ↦ (-y, x). -/
def perpendicular (v : Vec2) : Vec2 := ![-v 1, v 0]

/-!
### Matrix Types and Operations

2×2 matrices for linear transformations and their operations.
-/

/-- 2×2 matrix type for linear transformations. -/
abbrev Mat2 := Matrix (Fin 2) (Fin 2) Float

/-- Identity matrix. -/
instance : One Mat2 where
  one := !![1, 0; 0, 1]

/--
Matrix multiplication.
Uses custom notation to distinguish from other operations.
-/
def matMul (A B : Mat2) : Mat2 :=
  fun i j => A i 0 * B 0 j + A i 1 * B 1 j

/-- Custom infix notation for matrix multiplication. -/
infixl:70 "∘ₘ" => matMul

/-- Matrix-vector multiplication. -/
def mulVec (A : Mat2) (v : Vec2) : Vec2 :=
  fun i => (A i 0) * v 0 + (A i 1) * v 1

/-- Infix notation for matrix-vector multiplication. -/
infixl:65 "@" => mulVec

/-!
### Affine Transformations

Affine transformations combine linear transformations with translations.
They form the core geometric transformation system for Vizagrams.
-/

/--
Affine transformation represented as matrix A and translation vector b.
Transforms point x to Ax + b.

This is the fundamental transformation type in Vizagrams, enabling
all geometric manipulations while preserving affine properties.
-/
structure Mat2Vec2 where
  /-- Linear transformation component -/
  A : Mat2
  /-- Translation component -/
  b : Vec2
deriving Repr

/-!
### Coercion System

Smooth conversions between transformation types using the direct operations.
-/

/-- Treat translation vector as affine transformation. -/
instance : Coe Vec2 Mat2Vec2 where
  coe v := { A := 1, b := v }

/-- Treat linear transformation as affine transformation (no translation). -/
instance : Coe Mat2 Mat2Vec2 where
  coe M := { A := M, b := nullVec2 }

/-!
### Affine Map Operations

Direct operations for Mat2Vec2 transformations, providing the categorical structure.
-/

/-- Identity transformation (categorical identity morphism). -/
def identity : Mat2Vec2 := { A := 1, b := nullVec2 }

/-- Apply affine transformation to a point. -/
def apply (f : Mat2Vec2) (x : Vec2) : Vec2 := mulVec f.A x + f.b

/-- Compose two affine transformations following categorical law: (f ∘ g)(x) = f(g(x)).
    This satisfies:
    - Associativity: (f ∘ₜ g) ∘ₜ h = f ∘ₜ (g ∘ₜ h)
    - Identity: identity ∘ₜ f = f and f ∘ₜ identity = f
-/
def compose (f g : Mat2Vec2) : Mat2Vec2 :=
  { A := f.A ∘ₘ g.A, b := (f.A @ g.b) + f.b }

/-- Infix notation for transformation application. -/
infixr:75 " ▷ " => apply

/-- Infix notation for transformation composition. -/
infixl:80 " ∘ₜ " => compose

/-- Alternative composition via multiplication operator. -/
instance : HMul Mat2Vec2 Mat2Vec2 Mat2Vec2 where
  hMul f g := compose f g

/-!
### Standard Geometric Transformations

Common transformations used throughout Vizagrams.
These provide the building blocks for all geometric manipulations.
-/

/-- Translation by vector t. -/
def translate (t : Vec2) : Mat2Vec2 :=
  { A := 1, b := t }

/-- Uniform scaling by factor s. -/
def scale (s : Float) : Mat2Vec2 :=
  { A := !![s, 0.0; 0.0, s], b := nullVec2 }

/-- Rotation by angle θ (in radians, counterclockwise). -/
def rotate (θ : Float) : Mat2Vec2 :=
  let c := Float.cos θ
  let s := Float.sin θ
  { A := !![c, -s; s, c], b := nullVec2 }

/-- Reflection across line making angle θ with x-axis. -/
def reflect (θ : Float) : Mat2Vec2 :=
  let c := Float.cos (2 * θ)
  let s := Float.sin (2 * θ)
  { A := !![c, s; s, -c], b := nullVec2 }

/-!
### Convenience Functions

Higher-level operations built on the transformation primitives.
-/

/-- Apply rotation transformation to a vector. -/
def rotateVec2 (v : Vec2) (θ : Float) : Vec2 :=
  apply (rotate θ) v

/-- Point on ellipse with given angle and radii. -/
def pointOnEllipse (θ rx ry : Float) : Vec2 :=
  ![rx * Float.cos θ, ry * Float.sin θ]

/-- Angle of vector from positive x-axis (atan2 function). -/
def atan2pi (v : Vec2) : Float :=
  Float.atan2 (v 1) (v 0)

/-- Create transformation matrix for general 2D rotation around origin. -/
def rotationMatrix (θ : Float) : Mat2 :=
  let c := Float.cos θ
  let s := Float.sin θ
  !![c, -s; s, c]

/-- Create transformation matrix for scaling. -/
def scalingMatrix (sx sy : Float) : Mat2 :=
  !![sx, 0; 0, sy]

/-!
### Coordinate System Utilities

Functions for working with different coordinate systems and transformations.
-/

/-- Convert from polar coordinates (r, θ) to Cartesian. -/
def fromPolar (r θ : Float) : Vec2 :=
  ![r * Float.cos θ, r * Float.sin θ]

/-- Convert from Cartesian to polar coordinates. -/
def toPolar (v : Vec2) : Float × Float :=
  (vecNorm v, atan2pi v)

/-- Transform that maps unit square [0,1]² to given rectangle. -/
def rectTransform (corner : Vec2) (width height : Float) : Mat2Vec2 :=
  let scaling := { A := !![width, 0; 0, height], b := nullVec2 }
  let translation := translate corner
  translation ∘ₜ scaling

/-!
### Geometric Predicates and Tests

Functions for geometric reasoning and validation.
-/

/-- Check if two vectors are approximately equal (within floating point tolerance). -/
def approxEqual (v₁ v₂ : Vec2) (ε : Float := 1e-10) : Bool :=
  vecNorm (v₁ - v₂) < ε

/-- Check if vector is approximately zero. -/
def isZero (v : Vec2) (ε : Float := 1e-10) : Bool :=
  vecNorm v < ε

/-- Check if two vectors are approximately orthogonal. -/
def isOrthogonal (v₁ v₂ : Vec2) (ε : Float := 1e-10) : Bool :=
  Float.abs ⟪v₁, v₂⟫ < ε

/-- Check if transformation preserves orientation (determinant > 0). -/
def preservesOrientation (t : Mat2Vec2) : Bool :=
  let det := t.A 0 0 * t.A 1 1 - t.A 0 1 * t.A 1 0
  det > 0

end LinearAlgebra
