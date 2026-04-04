/-
Copyright (c) 2025 Henrique Borges. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Davi Barreira, Henrique Borges, Alexandre Rademaker
-/
import Vizagrams.Mark

/-!
# Free Monad for Compositional Graphics

This module implements the free monad structure `𝕋` (Tree) that provides compositional semantics
for graphical marks. It enables building complex diagrams through algebraic composition and
transformation operators.

## Core Concepts

The free monad `𝕋 α` represents a tree of operations that can be:
- **Pure values**: `𝕋.pure : α → 𝕋 α` — lift a value into the tree
- **Composition**: `𝕋.comp : 𝕋 α → 𝕋 α → 𝕋 α` — combine two trees (like `+`)
- **Transformation**: `𝕋.act : ℍ → 𝕋 α → 𝕋 α` — apply a transformation (like `*`)

## The ℍ Structure

`ℍ` (graphical transformation) bundles:
- `s : Style` — styling attributes (color, stroke, etc.)
- `g : Mat2Vec2` — geometric transformations (rotation, scaling, translation)

## Algebra and Evaluation

The free monad is "free" because it builds an abstract syntax tree of operations without
immediately evaluating them. The algebra `alg_θ` interprets this tree into concrete primitives:

```
𝕋 Mark → Array Prim
```

This deferred evaluation allows for optimization and reasoning about diagram structure.

## Usage Examples

```lean
-- Compose marks
let diagram := circle + square                  -- 𝕋.comp

-- Apply transformations
let rotated := rotate (π/4) * square            -- 𝕋.act

-- Combine both
let complex := (rotate (π/4) * square) + circle
```

## Theoretical Foundation

This follows the free monad construction from category theory:
- `𝕋` is the free monad over the functor `F`
- `η` (eta) is the unit of the monad (pure)
- `μ` (mu) is the multiplication (join/flatten)
- The monad laws hold by construction
-/

open GraphicalPrimitive
open GraphicalMark
open Sty
open LinearAlgebra

namespace FreeMonad

/--
**ℍ (Graphical Transformation)**: Combines styling and geometric transformations.

This structure represents both visual styling and spatial transformations that can be
applied to graphical marks. The name ℍ is chosen to represent a "homomorphism" or
transformation in the categorical sense.

**Components:**
- `s : Style` — Styling attributes (fill color, stroke color, stroke width)
- `g : Mat2Vec2` — Affine transformation (rotation, scaling, translation)

**Usage:**
```lean
let redRotated : ℍ := ℍ.mk {fill_color := Color.mk 1 0 0} (rotate (π/4))
let transformed := redRotated * myMark
```
-/
structure ℍ where
  s : Style
  g : Mat2Vec2

/-- Coerce a `Style` into `ℍ` by using the identity transformation (scale 1). -/
instance : Coe Style ℍ where
  coe st := ℍ.mk st (scale 1)

/-- Coerce a geometric transformation `Mat2Vec2` into `ℍ` with empty styling. -/
instance : Coe Mat2Vec2 ℍ where
  coe Ab := ℍ.mk {} Ab

/--
Multiplication of transformations composes styles (via `++`) while keeping the first transformation.
This allows chaining transformations like `h1 * h2`.
-/
instance : Mul ℍ where
  mul x y := ℍ.mk ( x.s ++ y.s) ( x.g )


universe u

/--
**𝕋 (Free Monad Tree)**: The central data structure for compositional graphics.

The free monad `𝕋 α` builds an abstract syntax tree of graphical operations:
- `pure x`: A leaf node containing a value of type `α`
- `comp t1 t2`: Composition node combining two trees (visualized as `t1 + t2`)
- `act h t`: Transformation node applying `ℍ` to a tree (visualized as `h * t`)

**Key Properties:**
- Deferred evaluation: operations build a tree without immediate rendering
- Compositional: combine simple marks into complex diagrams
- Algebraic: supports monadic operations (pure, bind)
- Universe polymorphic: works across different type universes

**Evaluation:**
The tree is interpreted into concrete primitives via the algebra `alg_θ : 𝕋 Mark → Array Prim`.
-/
inductive 𝕋 (α : Type u) where
  | pure : α → 𝕋 α
  | comp : 𝕋 α → 𝕋 α → 𝕋 α
  | act : ℍ → 𝕋 α → 𝕋 α


/--
Map a function over the values in a tree, preserving the tree structure.
This makes `𝕋` a functor.
-/
def 𝕋.map (f : α → β) (a : 𝕋 α) : 𝕋 β :=
  match a with
  | 𝕋.pure x => 𝕋.pure (f x)
  | 𝕋.comp x y => 𝕋.comp (𝕋.map f x) (𝕋.map f y)
  | 𝕋.act h x => 𝕋.act h (𝕋.map f x)

/-- `𝕋` is a functor. -/
instance : Functor 𝕋 where
  map := 𝕋.map

/--
**η (eta, unit)**: Lifts a value into the free monad.
This is the monadic `pure` operation.
-/
def η : α → 𝕋 α := fun a => 𝕋.pure a

/--
**μ (mu, multiplication)**: Flattens a nested tree `𝕋 (𝕋 α)` into `𝕋 α`.

This is the monadic join operation. It handles transformation composition:
when two transformations are nested (`act h (act h' t)`), they are composed
into a single transformation (`act (h * h') t`).
-/
def μ : 𝕋 (𝕋 α) → 𝕋 α
  | 𝕋.pure x => x
  | 𝕋.comp x y => 𝕋.comp (μ x) (μ y)
  | 𝕋.act h x =>
    match x with
      | 𝕋.act h' y => 𝕋.act (h * h') (μ y)
      | a => μ a

/--
Monadic bind for the free monad: composes `ma : 𝕋 α` with `f : α → 𝕋 β`.
Defined as `μ ∘ map f`.
-/
def free_bind : (𝕋 α) → (α → 𝕋 β) → (𝕋 β) :=
  fun ma f => (μ ∘ (𝕋.map f)) ma

/-- `𝕋` is a monad with `η` as unit and `free_bind` as bind. -/
instance : Monad 𝕋 where
  pure := η
  bind := free_bind

/--
Apply a graphical transformation `ℍ` to an array of primitives.
First applies the geometric transformation `h.g`, then the style `h.s`.
-/
def apply_h (h : ℍ) (prims : Array Prim) : Array Prim :=
  prims.map (fun p => h.s * (h.g * p))

/--
**alg_θ (Algebra Theta)**: The evaluation algebra for the free monad.

Interprets a tree of primitive arrays into a single array of primitives:
- `pure x`: Return the array as-is
- `comp x y`: Concatenate the arrays from both branches (using `⊕`)
- `act h x`: Apply transformation `h` to the evaluated array from `x`

This is a catamorphism (fold) over the tree structure.
-/
def alg_θ : 𝕋 (Array Prim) → Array Prim
  | 𝕋.pure x => x
  | 𝕋.comp x y => (alg_θ x) ⊕ (alg_θ y)
  | 𝕋.act h x => apply_h h (alg_θ x)

/--
**flat**: Evaluates a tree of marks into an array of primitives.

This is the main evaluation function that:
1. Maps `Mark.θ` over the tree to get `𝕋 (Array Prim)`
2. Evaluates the tree using the algebra `alg_θ` to get `Array Prim`

**Type signature:** `𝕋 Mark → Array Prim`

This flattens the compositional structure into drawable primitives.
-/
def flat (t : 𝕋 Mark) : Array Prim := alg_θ ((𝕋.map Mark.θ) t)

/-!
### Operator Instances

These instances provide intuitive syntax for building diagrams:
- `h * mark`: Apply transformation `h` to a mark (left multiplication)
- `mark * h`: Apply transformation `h` to a mark (right multiplication)
- `mark1 + mark2`: Compose two marks side-by-side
- Automatic coercion from `Mark` to `𝕋 Mark`
-/

/-- Left multiplication: `h * t` applies transformation `h` to tree `t`. -/
instance : HMul ℍ (𝕋 Mark) (𝕋 Mark) :=
  ⟨fun h t => 𝕋.act h t⟩

/-- Right multiplication: `t * h` applies transformation `h` to tree `t`. -/
instance : HMul (𝕋 Mark) ℍ (𝕋 Mark) :=
  ⟨fun t h => 𝕋.act h t⟩

/-- Automatically lift a `Mark` into the free monad `𝕋 Mark`. -/
instance : Coe Mark (𝕋 Mark) where
  coe m := 𝕋.pure m

/-- Composition: `t1 + t2` combines two trees. -/
instance : HAdd (𝕋 Mark) (𝕋 Mark) (𝕋 Mark) where
  hAdd m1 m2 := 𝕋.comp m1 m2

/-- Composition: `mark + tree` lifts mark and composes. -/
instance : HAdd Mark (𝕋 Mark) (𝕋 Mark) where
  hAdd m t := 𝕋.comp (𝕋.pure m) t

/-- Composition: `tree + mark` lifts mark and composes. -/
instance : HAdd (𝕋 Mark) Mark (𝕋 Mark) where
  hAdd t m := 𝕋.comp t (𝕋.pure m)

/-- Universe-polymorphic left multiplication. -/
instance : HMul ℍ (𝕋 Mark.{u}) (𝕋 Mark.{u}) :=
  ⟨fun h t => 𝕋.act h t⟩

/-- Universe-polymorphic right multiplication. -/
instance : HMul (𝕋 Mark.{u}) ℍ (𝕋 Mark.{u}) :=
  ⟨fun t h => 𝕋.act h t⟩

/-- Universe-polymorphic coercion from `Mark` to `𝕋 Mark`. -/
instance : Coe Mark.{u} (𝕋 Mark.{u}) where
  coe m := 𝕋.pure m

/-!
### Universe Polymorphism

To support composition across different type universes, we provide `ulift` and
universe-polymorphic instances. This allows combining marks from different
universe levels seamlessly.
-/

/--
**ulift**: Lift a tree from universe `u+1` to universe `max u v + 1`.
This enables composition of marks across different universe levels.
-/
def 𝕋.ulift (a : 𝕋.{u+1} Mark ) : 𝕋.{(max u v) + 1} (Mark) :=
  match a with
  | .pure x => .pure x.ulift
  | .comp s t => .comp s.ulift t.ulift
  | .act h t  => .act h (𝕋.ulift t)

/-- Universe-polymorphic addition: `𝕋 Mark.{u} + 𝕋 Mark.{v}`. -/
instance : HAdd (𝕋 Mark.{u}) (𝕋 Mark.{v}) (𝕋 Mark.{max u v}) where
  hAdd m n := 𝕋.comp m.ulift n.ulift

/-- Universe-polymorphic addition: `Mark.{u} + 𝕋 Mark.{v}`. -/
instance : HAdd Mark.{u} (𝕋 Mark.{v}) (𝕋 Mark.{max u v}) where
  hAdd m n := 𝕋.comp (𝕋.pure m.ulift) n.ulift

/-- Universe-polymorphic addition: `𝕋 Mark.{u} + Mark.{v}`. -/
instance : HAdd (𝕋 Mark.{u}) Mark.{v} (𝕋 Mark.{max u v}) where
  hAdd m n := 𝕋.comp (m.ulift) (𝕋.pure n.ulift)

/-- Universe-polymorphic addition: `Mark.{u} + Mark.{v}`. -/
instance : HAdd Mark.{u} Mark.{v} (𝕋 Mark.{max u v}) where
  hAdd m n := 𝕋.comp (𝕋.pure (Mark.ulift m)) (𝕋.pure (Mark.ulift n))

end FreeMonad
