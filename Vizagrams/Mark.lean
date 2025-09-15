/-
Copyright (c) 2025 Henrique Borges. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Davi Barreira, Henrique Borges
-/
import Vizagrams.Prim

/-!
# Graphical Marks for Vizagrams

This module implements the foundational concept of graphical marks as defined in the categorical
framework for data visualization. A graphical mark is fundamentally a tuple (A, θ_A) where:
- A is a type representing the mark's parameters
- θ_A : A → Array Prim is a function describing how to render the mark as primitives

## Conceptual Hierarchy

1. **Prim** (Primitive): Basic geometric shapes with styling
2. **Mark** (This module): Abstractions over collections of primitives
3. **𝕋 Mark** (Free Monad): Tree structures of marks for complex diagrams

## Key Concepts

- **MarkInterface**: Type class defining how types can be rendered as primitive arrays
- **Mark**: Existential wrapper for any type implementing MarkInterface
- **θ function**: Core rendering function transforming marks into primitive arrays
- **Composition**: How marks combine to form more complex visual elements

## Theoretical Foundation

Following the categorical theory:
- Marks are abstractions over Array Prim
- θ provides semantic interpretation: abstract mark → concrete primitives
- Later, 𝕋 Mark will use these basic marks to build tree diagrams

-/

namespace GraphicalMark

open LinearAlgebra
open Sty ProofWidgets.Svg
open GeometricPrimitive
open GraphicalPrimitive

-- Ensure we can work across universe levels
set_option autoImplicit true
universe u v

/-!
### MarkInterface: The Foundation

The `MarkInterface` type class is the cornerstone of the mark system. It defines
how any type can be interpreted as a visual mark by providing the θ (theta) function
that converts abstract mark values into concrete arrays of graphical primitives.
-/

/--
Type class defining how a type can be rendered as an array of graphical primitives.
The θ function is the semantic map from abstract marks to concrete visual elements.

This follows the mathematical definition where a mark is a tuple (A, θ_A) with
θ_A : A → Array Prim providing the rendering semantics.
-/
class MarkInterface (α : Type u) where
  /--
  The theta function: transforms a mark value into an array of primitives.
  This is the core semantic function defining what the mark looks like when rendered.
  -/
  θ : α → Array Prim

/-!
### Mark Type: Existential Wrapper

The `Mark` type provides a uniform interface for heterogeneous collections of marks.
It wraps any type that implements `MarkInterface`, enabling type-safe composition
of different mark types in the same data structure.
-/

/--
A graphical mark represented as an existential type containing any type
that implements the MarkInterface. This enables heterogeneous mark collections
while preserving type safety.
-/
structure Mark where
  {Carrier : Type u}
  [inst : MarkInterface Carrier]
  value : Carrier

/-!
### Universe Level Management

For combining marks across different type universes, we provide lifting operations.
This is essential for compositional mark construction.
-/

/--
Lift a mark to work across universe levels.
Enables combination of marks defined at different universe levels.
-/
def Mark.ulift (m : Mark.{u}) : Mark.{max u v} where
  Carrier := ULift m.Carrier
  value := ULift.up m.value
  inst := {
    θ := fun (lifted : ULift m.Carrier) => m.inst.θ (ULift.down lifted)
  }

/--
Extract the array of primitives from a Mark using its θ function.
This is the canonical way to render any mark.
-/
def Mark.θ (m : Mark) : Array Prim := m.inst.θ m.value

/-!
### Fundamental Mark Instances

Basic types that can serve as marks in the system.
-/

/-- Empty mark type representing no visual content. -/
inductive NilMark : Type
  | mk : NilMark

/-- NilMark renders as empty array (identity for composition). -/
instance : MarkInterface NilMark where
  θ _ := #[]

/-- Unit type as empty mark alternative. -/
instance : MarkInterface Unit where
  θ _ := #[]

/-- Individual primitives are trivial marks. -/
instance : MarkInterface Prim where
  θ p := #[p]

/-- Arrays of primitives are direct mark representations. -/
instance : MarkInterface (Array Prim) where
  θ ps := ps

/-!
### Coercion System

Smooth conversions between representational levels in the hierarchy.
-/

/-- Convert Mark to its primitive array representation. -/
instance : Coe Mark (Array Prim) where
  coe m := m.θ

/-- Lift primitive to Mark level. -/
instance : Coe Prim Mark where
  coe p := ⟨p⟩

/-- Lift empty mark to Mark level. -/
instance : Coe NilMark Mark where
  coe n := ⟨n⟩

/-- Lift Unit to Mark level. -/
instance : Coe Unit Mark where
  coe u := ⟨u⟩

/-- Lift primitive arrays to Mark level. -/
instance : Coe (Array Prim) Mark where
  coe ps := ⟨ps⟩

/-!
### Mark Composition Operations

These operations define how marks combine at the primitive level.
Later, 𝕋 Mark will provide higher-level tree-based composition.
-/

/-- Concatenate arrays from two marks. -/
instance : HAdd Mark Mark (Array Prim) where
  hAdd m₁ m₂ := m₁.θ ++ m₂.θ

/-- Prepend primitive to mark's primitive array. -/
instance : HAdd Prim Mark (Array Prim) where
  hAdd p m := #[p] ++ m.θ

/-- Append primitive to mark's primitive array. -/
instance : HAdd Mark Prim (Array Prim) where
  hAdd m p := m.θ ++ #[p]

/-- Concatenate primitive array with mark's primitives. -/
instance : HAdd (Array Prim) Mark (Array Prim) where
  hAdd ps m := ps ++ m.θ

/-- Concatenate mark's primitives with primitive array. -/
instance : HAdd Mark (Array Prim) (Array Prim) where
  hAdd m ps := m.θ ++ ps

/-!
### Transformation Operations

Apply geometric and style transformations to marks by transforming
their underlying primitive arrays.
-/

/-- Apply geometric transformation to mark. -/
instance : HMul Mat2Vec2 Mark (Array Prim) where
  hMul g m := g * m.θ

/-- Apply style transformation to mark. -/
instance : HMul Style Mark (Array Prim) where
  hMul s m := s * m.θ

/-- Apply transformation to mark then convert back to Mark. -/
instance : HMul Mat2Vec2 (Array Prim) Mark where
  hMul g ps := ⟨g * ps⟩

/-- Apply style to primitive array then convert back to Mark. -/
instance : HMul Style (Array Prim) Mark where
  hMul s ps := ⟨s * ps⟩

/-!
### Utility Functions

Helper functions for mark creation and manipulation.
-/

/-- Create a Mark from any type implementing MarkInterface. -/
def mk {α : Type u} [MarkInterface α] (value : α) : Mark := ⟨value⟩

/-- Check if a Mark contains no visual content (empty primitive array). -/
def Mark.isEmpty (m : Mark) : Bool := m.θ.size = 0

/-- Get the number of primitives in a Mark. -/
def Mark.size (m : Mark) : Nat := m.θ.size

/-!
### Common Mark Implementations

Ready-to-use mark types for common visual elements.
-/

/-- Simple circle mark with customizable properties. -/
structure CircleMark where
  radius : Float := 1.0
  center : Vec2 := ![0, 0]
  style : Style := {}
  deriving Repr

instance : MarkInterface CircleMark where
  θ c := #[c.style * Prim.circle c.radius c.center]

/-- Text mark for labels and annotations. -/
structure TextMark where
  content : String
  position : Vec2 := ![0, 0]
  size : Float := 1.0
  style : Style := {}
  deriving Repr

instance : MarkInterface TextMark where
  θ t := #[t.style * Prim.text t.position t.content t.size]

/-- Composite mark that groups multiple primitives. -/
structure GroupMark where
  primitives : Array Prim
  deriving Repr

instance : MarkInterface GroupMark where
  θ g := g.primitives

/-!
### Advanced Mark Construction

Functions for building complex marks from simpler ones.
-/

/-- Create a mark by applying a function to transform primitive arrays. -/
def transformMark (f : Array Prim → Array Prim) (m : Mark) : Mark :=
  ⟨f m.θ⟩

/-- Combine multiple marks into a single composite mark. -/
def combineMark (marks : Array Mark) : Mark :=
  ⟨marks.foldl (fun acc m => acc ++ m.θ) #[]⟩

/-- Apply uniform styling to all primitives in a mark. -/
def styleMark (style : Style) (m : Mark) : Mark :=
  transformMark (style * ·) m

end GraphicalMark
