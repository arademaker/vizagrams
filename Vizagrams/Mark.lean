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

```
Prim (Primitive)     →  Array Prim  →  Visual Output
     ↑                       ↑
Mark (This module)    →  Array Prim  →  Visual Output
     ↑                       ↑
𝕋 Mark (Free Monad)  →  Array Prim  →  Visual Output
```

## Key Design Principles

1. **Type → Drawing Pipeline**: Any type can become drawable by implementing `MarkInterface`
2. **Ergonomic Composition**: Rich operator overloading (`+`, `*`) for intuitive syntax
3. **Automatic Coercion**: Seamless conversion between related types
4. **Universe Polymorphism**: Support for marks across different type universes

## Usage Examples

```lean
-- Basic mark creation and composition
let circle : Mark := myPrim                    -- Automatic coercion
let combined := mark1 + mark2                  -- Composition operator
let transformed := transform * mark            -- Transformation operator

-- All operations work seamlessly with type inference
```

## Theoretical Foundation

Following the categorical theory from "Data Visualization from a Category Theory Perspective":
- Marks are abstractions over Array Prim
- θ provides semantic interpretation: abstract mark → concrete primitives
- Composition forms a monoid structure
- Transformations preserve the categorical structure
-/
namespace GraphicalMark

open LinearAlgebra
open GeometricPrimitive
open GraphicalPrimitive

-- Explicit universe declaration for polymorphic operations
universe u v

/-!
### Core Type Class: MarkInterface

The foundation of the mark system. Any type implementing this interface
can be rendered as visual primitives through the θ (theta) function.
-/

/--
**MarkInterface**: Defines how a type can be transformed into drawable primitives.

A type `α` can be drawn if there exists a function `θ : α → Array Prim`.
This follows the mathematical definition where a mark is a tuple (A, θ_A).

**Mathematical Interpretation:**
- A ∈ Type_u (the parameter space)
- θ_A : A → Array Prim (the rendering function)
- (A, θ_A) forms a graphical mark in our categorical framework

**Pipeline:** `α → Array Prim → Drawing`
-/
class MarkInterface (a : Type u) where
  /-- The theta function: transforms mark parameters into primitive arrays -/
  θ : a -> Array Prim

/-!
### Existential Mark Type

The `Mark` type provides a uniform interface for heterogeneous collections of marks.
It wraps any type implementing `MarkInterface`, enabling type-safe composition
while hiding the underlying type complexity.
-/

/--
**Mark**: An existential wrapper for any drawable type.

A Mark encapsulates:
- `T`: The underlying parameter type (hidden from users)
- `inst`: The MarkInterface instance providing θ
- `val`: The concrete parameter values

This enables heterogeneous mark collections while preserving type safety.

**Design Benefits:**
- **Type Erasure**: Users work with uniform `Mark` type
- **Extensibility**: New mark types integrate seamlessly
- **Composition**: All marks compose uniformly regardless of underlying type
-/
structure Mark where
  {T : Type u}                    -- Hidden parameter type
  [inst : MarkInterface T]        -- Rendering capability
  val : T                        -- Concrete parameter values

instance : Repr Mark where
  reprPrec m := reprPrec (m.inst.θ m.val)

/-!
### Universe Level Management

Support for combining marks defined at different universe levels.
Essential for compositional mark construction across type hierarchies.
-/

/--
**Mark.ulift**: Lift mark to higher universe level.

Enables combination of marks defined at different universe levels.
This is crucial for building complex diagrams that combine marks
from different abstraction levels.

**Category Theory Note:** This preserves the categorical structure
while enabling cross-universe composition.
-/
def Mark.ulift (x : Mark.{u}) : Mark.{max u v} where
  T := ULift x.T
  val := ULift.up x.val
  inst := {
    θ := fun (v : ULift x.T) => x.inst.θ (ULift.down v)
  }

/-!
### Core Mark Operations
-/

/--
**Mark.θ**: Extract primitive array from any mark.

This is the canonical rendering function that converts any Mark
into its visual representation as an Array Prim.

**Mathematical Interpretation:**
Projects the existential type back to the primitive space:
`∃T. (T, θ_T, val_T) → Array Prim`
-/
def Mark.θ : Mark → Array Prim := fun m => m.inst.θ m.val

/-!
### Fundamental Mark Instances

Basic types that can serve as marks. These provide the foundation
for more complex mark constructions.
-/

/--
**Nil**: Empty mark type representing visual void.

Analogous to the empty set ∅ in category theory.
Serves as the identity element for mark composition.
-/
inductive Nil : Type
  | mk : Nil

/-- **Nil Instance**: Renders as empty array (composition identity) -/
instance : MarkInterface Nil where
  θ _ := #[]

/-- **Unit Instance**: Alternative empty mark using built-in Unit type -/
instance : MarkInterface Unit where
  θ _ := #[]

/-- **Prim Instance**: Individual primitives are trivial marks -/
instance : MarkInterface Prim where
  θ p := #[p]

/-!
### Coercion System: Seamless Type Conversions

Rich coercion system enabling intuitive syntax. Users can write
natural code without explicit conversions between related types.

**Design Philosophy:** Make the common cases effortless while preserving type safety.
-/

/-- **Mark → Array Prim**: Extract primitives (every Mark can render) -/
instance : Coe Mark (Array Prim) where
  coe m := m.θ

/-- **Prim → Mark**: Lift primitive to mark level -/
instance : Coe Prim Mark where
  coe p := { val := p }

/-- **Nil → Mark**: Lift empty mark to uniform interface -/
instance : Coe Nil Mark where
  coe m := { val := m }

/-- **Unit → Mark**: Lift unit to mark level -/
instance : Coe Unit Mark where
  coe m := { val := m }

/-!
### Composition Operations: The `+` Operator

Rich composition system using the `+` operator for intuitive mark combination.
This implements the monoidal structure of mark composition.

**Mathematical Foundation:**
- Operation: `+` (composition)
- Identity: `Nil` (empty mark)
- Associativity: `(a + b) + c = a + (b + c)`
- Commutativity: Generally `a + b ≠ b + a` (order matters for rendering)
-/

/-- **Prim + Mark**: Prepend primitive to mark's primitives -/
instance : HAdd Prim Mark (Array Prim) where
  hAdd p1 p2 := #[p1] ++ p2.θ

/-- **Mark + Prim**: Append primitive to mark's primitives -/
instance : HAdd Mark Prim (Array Prim) where
  hAdd p1 p2 := p1.θ ++ #[p2]

/-- **Array Prim + Mark**: Prepend primitive array to mark -/
instance : HAdd (Array Prim) Mark (Array Prim) where
  hAdd p1 p2 := p1 ++ p2.θ

/-- **Mark + Mark**: Core composition operation -/
instance : HAdd Mark Mark (Array Prim) where
  hAdd p1 p2 := p1.θ ++ p2.θ

/-!
### Transformation Operations: The `*` Operator

Geometric and style transformation system using the `*` operator.
This implements the action of the transformation group on marks.

**Mathematical Foundation:**
- Geometric transformations form a group under composition
- Style transformations form a monoid under right-biased union
- The `*` operator applies these transformations to mark content

**Categorical Interpretation:**
- Transformations are morphisms in the category of geometric objects
- `*` implements the group action on the mark space
- Preserves the compositional structure of marks
-/

/-- **Mat2Vec2 * Mark**: Apply geometric transformation to mark -/
instance : HMul Mat2Vec2 Mark (Array Prim) where
  hMul g M := g * M.θ

/-- **Mark * Mat2Vec2**: Apply transformation (alternative syntax) -/
instance : HMul Mark Mat2Vec2 (Array Prim) where
  hMul M g := g * M.θ

/-!
### API Summary

**Core Functions:**
- `Mark.θ : Mark → Array Prim` - Extract primitives from any mark
- `Mark.ulift : Mark.{u} → Mark.{max u v}` - Universe level lifting

**Composition Operators:**
- `mark1 + mark2` - Combine two marks
- `prim + mark` / `mark + prim` - Mix primitives and marks
- `array + mark` - Combine primitive arrays with marks

**Transformation Operators:**
- `transform * mark` - Apply geometric/style transformation
- `mark * transform` - Alternative syntax for transformation

**Automatic Coercions:**
- `Prim → Mark` - Primitives automatically become marks
- `Nil → Mark` - Empty marks integrate seamlessly
- `Unit → Mark` - Unit type as empty mark
- `Mark → Array Prim` - Marks automatically render when needed

This design enables highly ergonomic syntax while maintaining mathematical rigor
and type safety throughout the system.
-/

end GraphicalMark
