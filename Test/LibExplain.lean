/-
# Vizagrams Library Explained

This file provides a comprehensive walkthrough of the Vizagrams library,
demonstrating the core concepts, types, and operations available for creating
data visualizations in Lean 4.

## Overview

Vizagrams is built on category-theoretic foundations, providing a compositional
approach to data visualization. This tutorial covers:
1. The hierarchy of drawable objects
2. Geometric transformations
3. Styling and composition
4. Envelopes and bounding boxes
5. Creating custom marks
-/

import Vizagrams.FreeMonad
import Vizagrams.VizBackend

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad
open LinearAlgebra

/-!
## 1. Understanding Graphical Objects

Vizagrams provides a hierarchy of four drawable object types:

1. **Prim** (Primitive): The basic geometric shapes with styling
2. **Array Prim**: Ordered collections of primitives
3. **Mark**: Types implementing `MarkInterface` with a function `θ : Type → Array Prim`
4. **𝕋 Mark**: Free monad over marks, enabling compositional diagram construction

### The Type Hierarchy

```
Prim  →  Array Prim  →  Mark  →  𝕋 Mark
  ↓         ↓            ↓         ↓
(base)  (collection)  (semantic) (compositional)
```

Let's explore how these types relate and convert between each other.
-/

-- Drawing a circle as a primitive
def circleₚ : Prim := new_circle 1 ![0,0]  -- Using new_circle from VizBackend
#eval circleₚ
/-
Output:
{ geom := Geom.circle 1.000000 ![0.000000, 0.000000],
  style := { stroke_color := none, stroke_width := none, fill_color := some (0.000000, 0.000000, 0.000000) } }
-/

-- Verify that circleₚ can be coerced to Array Prim
#check (circleₚ : Array Prim)  -- #[circleₚ] : Array Prim

def circleₐ : Array Prim := circleₚ
#eval circleₐ
/-
Output:
#[{ geom := Geom.circle 1.000000 ![0.000000, 0.000000],
    style := { stroke_color := none,
               stroke_width := none,
               fill_color := some (0.000000, 0.000000, 0.000000) } }]
-/

-- Coercing Prim to Mark
#check (circleₚ : Mark)  -- Mark.mk circleₚ : Mark
def circleₘ : Mark := circleₚ

/-
**Coercion Chain: Prim → Mark**

We have a coercion `Prim → Mark`, but not yet `Array Prim → Mark`.
This design choice reflects that users typically work with `Mark` or `𝕋 Mark`,
not directly with `Array Prim`.
-/

-- Coercing Prim to 𝕋 Mark
#check (circleₚ : FreeMonad.𝕋 Mark)  -- FreeMonad.𝕋.pure (Mark.mk circleₚ) : FreeMonad.𝕋 Mark

/-
**Understanding the Prim → 𝕋 Mark Transformation**

The transformation happens in two steps:
1. `Prim` → `Mark` (via Mark.mk)
2. `Mark` → `𝕋 Mark` (via pure)

### What is a Monad?

A monad is a structure for composing functions that return values with effects:

```lean
class Monad (m : Type → Type) where
  pure : α → m α            -- Inject pure value into monadic context
  bind : m α → (α → m β) → m β  -- Compose monadic functions
```

In Vizagrams, `𝕋 Mark` represents a tree of graphical operations.
The `pure` operation creates a leaf node containing a single mark.
-/

#check (circleₘ : FreeMonad.𝕋 Mark)  -- FreeMonad.𝕋.pure circleₘ : FreeMonad.𝕋 Mark

def 𝕋circle : FreeMonad.𝕋 Mark := circleₘ

/-!
## 2. Rendering Functions

VizBackend provides two main rendering functions:
- `draw_svg (Array Prim) (Frame)`: Render primitives directly
- `draw (𝕋 Mark) (Frame)`: Render a diagram tree
- `draw₁ (𝕋 Mark)`: Automatically compute optimal frame from bounding box

**Note**: Due to coercions, `draw` works with `Prim`, `Mark`, and `𝕋 Mark`.
However, it doesn't work directly with `Array Prim` since there's no
coercion `Array Prim → 𝕋 Mark`.
-/

#html draw_svg circleₚ
#html draw circleₚ

/-
The following would fail:
```lean
#html draw circleₐ
-- Error: application type mismatch
--   circleₐ has type Array Prim : Type
--   but is expected to have type FreeMonad.𝕋 Mark : Type 1
```
-/

#html draw circleₘ
#html draw 𝕋circle

/-!
## 3. Geometric Transformations

Available transformations:
- **Translation**: Move objects in space
- **Rotation**: Rotate objects around the origin
- **Scaling**: Resize objects
- **Style transformations**: Modify visual appearance
-/

-- ### Translation on Prim
def t_r₁ : Mat2Vec2 := translate ![3,0]  -- Translate 3 units to the right

-- Transformations are applied to the left of the object
#eval t_r₁ * circleₚ
def circleₚtoright := t_r₁ * circleₚ
#check circleₚtoright  -- circleₚtoright : Prim

-- Visualize the difference
#html draw_svg circleₚ
#html draw_svg circleₚtoright

-- Combine primitives using ⊕ (array concatenation)
#check circleₚ ⊕ circleₚtoright  -- circleₚ ⊕ circleₚtoright : Array Prim
def two_circles := circleₚ ⊕ circleₚtoright
#html draw_svg two_circles

-- Note: Cannot use `draw` with Array Prim (no coercion Array Prim → 𝕋 Mark)

-- ### Translation on Array Prim
#eval t_r₁ * two_circles
#html draw_svg (t_r₁ * two_circles)

/-
**Transformation on Arrays**

Applying a transformation to `Array Prim` applies it element-wise:
```
t * #[p₁, p₂] = #[t * p₁, t * p₂]
```
-/

-- ### Translation on Mark
#check (t_r₁ * circleₘ)  -- t_r₁ * circleₘ : Array Prim

/-
**Mark Transformation Behavior**

When applying a transformation to a `Mark`, it's converted to `Array Prim`:
```lean
instance : HMul Mat2Vec2 Mark (Array Prim) where
  hMul g M := g * M.θ
```

This shows that `Mark` alone is "unstable" — most operations on `Mark`
return `Array Prim`. The proper compositional interface uses `𝕋 Mark`.
-/

#html draw_svg (t_r₁ * circleₘ)
def circleₘtoright := t_r₁ * circleₘ
-- Since circleₘtoright is Array Prim, we need to convert circleₘ to Array Prim too
def twoMarkCircles := circleₘtoright ⊕ (circleₘ : Array Prim)
#check twoMarkCircles  -- twoMarkCircles : Array Prim
#html draw_svg twoMarkCircles

-- ### Rotation
def g_45 : Mat2Vec2 := rotate (π/4)

-- Create a square to see rotation effects clearly
def square₀ : Prim := new_polygon #[![0.7,0.7], ![-0.7,0.7], ![-0.7,-0.7], ![0.7,-0.7]]
#html draw_svg square₀
#check g_45 * square₀  -- g_45 * square₀ : Prim

def squareᵣ := g_45 * square₀
#html draw_svg squareᵣ

-- Rotation on Array Prim (element-wise application)
def square₁ : Prim := t_r₁ * square₀
def twoSquares := square₀ ⊕ square₁
#html draw_svg (g_45 * twoSquares)

/-
**Important: Rotation around origin**

Notice the apparent translation effect when rotating. This occurs because
rotation is performed around the origin (0,0), not the object's center.
To rotate around an object's center, combine translation operations.
-/

-- ### Scaling
def scale2 : Mat2Vec2 := scale 2
def bigSquare := scale2 * square₀
#html draw_svg bigSquare
#html draw_svg (scale2 * twoSquares)

-- Composing transformations using function composition
#html draw_svg ((scale2 ∘ₜ g_45 ∘ₜ t_r₁) * square₀)

/-!
## 4. Style Transformations

Styles are defined in `Style.lean`:
```lean
structure Style where
  stroke_color := (none : Option Color)
  stroke_width := (none : Option StyleSize)
  fill_color   := (none : Option Color)
```

Styles compose using the `++` operator (right-biased merge).
-/

def borderToBlue : Sty.Style := {stroke_color := Color.mk 0 0 1}
#html draw_svg (borderToBlue * bigSquare)

def size : Sty.StyleSize := .px 10
def bigBorder : Sty.Style := {stroke_width := size}
#html draw_svg (bigBorder * (borderToBlue * bigSquare))

def toBlue : Sty.Style := {fill_color := Color.mk 0 0 1}
#html draw_svg (toBlue * bigSquare)

/-
**Style Composition with `++`**

After refactoring, styles compose using `++`:
```lean
instance : Append Style where
  append s1 s2 := {
    stroke_color := s2.stroke_color.orElse fun () => s1.stroke_color
    stroke_width := s2.stroke_width.orElse fun () => s1.stroke_width
    fill_color := s2.fill_color.orElse fun () => s1.fill_color
  }
```

The right-hand style overrides the left. When applying `toBlue * bigSquare`,
nothing changes because `new_polygon` pre-sets `fill_color`.
-/

def newSquare : Prim := {geom := Geom.polygon #[![0,0], ![2,0], ![2,2], ![0,2]], style := {}}
#eval newSquare
#html draw_svg newSquare  -- Invisible (no fill or stroke)
#html draw_svg (toBlue * newSquare)

-- Composing multiple styles
def redBorder : Sty.Style := {stroke_color := Color.mk 1 0 0}
#eval redBorder ++ toBlue
#html draw_svg ((redBorder ++ toBlue) * (bigBorder * newSquare))
#html draw_svg (g_45 * ((redBorder ++ toBlue) * (bigBorder * newSquare)))

/-!
## 5. Envelopes and Bounding Boxes

**Envelope**: Given a direction vector, the envelope of a diagram in that
direction is the minimum distance from the origin to the separating line
(the line that divides space into a half containing the diagram and an empty half).

Envelopes enable:
1. Computing bounding boxes
2. Positioning diagrams relative to each other
-/

open Envelope
#check (square₀ : Geom)  -- Coercion Prim → Geom for envelope operations
#eval (envelope square₀ ![1,1])

-- Computing bounding box by evaluating envelope in all directions
def boundingBox_square₀ := bounding_box_prim square₀
#check boundingBox_square₀
#html draw_svg square₀ (BoundingBox.toFrame boundingBox_square₀)

-- The bounding box of an axis-aligned square is the square itself
def bb_s45 := bounding_box_prim (g_45 * square₀)
#html draw_svg (g_45 * square₀) (BoundingBox.toFrame bb_s45)
-- After rotation, the square doesn't fill the entire bounding box

-- Bounding box for Array Prim
def bb_2s := bounding_box_prims twoSquares
#html draw_svg twoSquares (BoundingBox.toFrame bb_2s)
def bb_2s45 := bounding_box_prims (g_45 * twoSquares)
#html draw_svg (g_45 * twoSquares) (BoundingBox.toFrame bb_2s45)

/-
### Positioning diagrams using envelopes

To position diagram D₂ adjacent to D₁ in direction v:
1. Compute d₁ = envelope(D₁, v)
2. Compute d₂ = envelope(D₂, -v)
3. Offset = d₁ + d₂
4. Translate D₂ by (offset · v)

This ensures D₂'s closest face (in direction -v) touches D₁'s furthest face (in direction v).
-/

def h₁ : Vec2 := normalize ![0,10]

-- Calculate how far the first diagram extends in direction h₁
def limite_d₁ : Float := envelope (scale2 * circleₚ) h₁

-- Calculate how far the second diagram extends in the opposite direction
def limite_d₂ : Float := envelope circleₚ (-h₁)

-- Total offset needed for adjacency
def offset_h₁ : Float := limite_d₁ + limite_d₂

-- Scalar multiplication: scale the unit vector by the offset distance
def position : Vec2 := ![offset_h₁ * (h₁ 0), offset_h₁ * (h₁ 1)]

#eval position
#html draw_svg ((scale2 * circleₚ) ⊕ ((translate position) * circleₚ))

def diagrama₁ := ((scale2 * circleₚ) ⊕ ((translate position) * circleₚ))
#check diagrama₁
#eval diagrama₁
def bb_d := bounding_box_prims diagrama₁
#html draw_svg diagrama₁ (BoundingBox.toFrame bb_d)

/-!
## 6. Envelope-Based Positioning Operators

Vizagrams provides convenient operators for positioning:
- `→`: Position to the right
- `←`: Position to the left
- `↑`: Position above
- `↓`: Position below

These operators use envelopes internally to compute proper spacing.
-/

def circleₚpositioned := envelope_position_prim circleₚ ![1,1] circleₚ
#html draw_svg (circleₚ ⊕ circleₚpositioned)

#html draw_svg (circleₚ → circleₚ → circleₚ)
#html draw_svg (circleₚ → circleₚ ↑ square₀)

-- Positioning with custom spacing using →[gap]
#html draw_svg (circleₚ →[0.5] circleₚ →[0.5] circleₚ)

-- Example: Stacking circles recursively
def stackCircles : Nat → Float → Array Prim
| 0,          _ => #[]
| Nat.succ n, gap =>
  let prev := stackCircles n gap
  if prev.isEmpty then
    #[circleₚ]
  else
    prev →[gap] #[circleₚ]

#html draw_svg (circleₚ →[0.5] circleₚ →[0.5] circleₚ)

def d := stackCircles 5 0.5
#html draw_svg d

/-!
## 7. Recursive Fractal Example: Sierpinski Triangle
-/

-- Geometric constants for equilateral triangle with side 1
def h : Float := (3 / 2 : Float).sqrt

-- Base triangle primitive
def triₚ : Prim := new_polygon #[![0,0], ![1,0], ![0.5,h]]

/-
Recursive construction of Sierpinski triangle:
- Base case (n=0): Single triangle
- Recursive case (n+1): Three copies of order-n triangle, scaled to ½,
  positioned at (0,0), (0.5,0), and (0.25,h/2)
-/
def sierpinskiPrims : Nat → Array Prim
| 0   => #[triₚ]
| n+1 =>
  let prev := sierpinskiPrims n
  -- Scale everything by 0.5
  let scaled := prev.map (fun p => {geom := scale 0.5 * p.geom, style := p.style})
  -- Three positions
  let t1 := scaled
  let t2 := scaled.map (fun p => {geom := translate ![0.5,0] * p.geom, style := p.style})
  let t3 := scaled.map (fun p => {geom := translate ![0.25,h/2] * p.geom, style := p.style})
  -- Concatenate
  t1 ++ t2 ++ t3

#html draw_svg (sierpinskiPrims 4) (BoundingBox.toFrame (bounding_box_prims (sierpinskiPrims 4)))

/-!
## 8. Transformations on 𝕋 Mark

In FreeMonad, transformations use the ℍ structure:
```lean
structure ℍ where
  s : Style        -- Styling attributes
  g : Mat2Vec2     -- Geometric transformation
```

This structure governs transformations on `𝕋 Mark` objects.
-/

-- Working with 𝕋 Mark
#check 𝕋circle
#html draw 𝕋circle

-- Applying transformations to 𝕋 Mark
def 𝕋translation (x : Vec2) : FreeMonad.ℍ := ℍ.mk {} (translate x)
def x : Vec2 := ![2,0]
#html draw (𝕋translation x * 𝕋circle)
#check (𝕋translation x * 𝕋circle)

/-
### Understanding the Tree Structure

Intuitively, composing marks builds a tree:

Initial state:
```
    circle
```

After applying 𝕋translation:
```
    ℍ.act
      \
    circle
```

Evaluation flattens the tree:
```lean
def flat (t : 𝕋 Mark) : Array Prim :=
  alg_θ (𝕋.map Mark.θ t)
```
-/

def twoCircles : FreeMonad.𝕋 Mark := 𝕋circle + (𝕋translation x * 𝕋circle)
#html draw twoCircles

/-
Tree structure of twoCircles:
```
       𝕋.comp
      /      \
 𝕋circle    𝕋.act ℍ
               \
             𝕋circle
```
-/

-- Other geometric transformations
def 𝕋rotate (y : Float) : FreeMonad.ℍ := ℍ.mk {} (rotate y)
def 𝕋scale (z : Float) : FreeMonad.ℍ := ℍ.mk {} (scale z)

def 𝕋square : FreeMonad.𝕋 Mark := square₀

#html draw (𝕋rotate (π/4) * 𝕋square)
#html draw (𝕋scale 0.3 * 𝕋circle)

/-
### Style Transformations on 𝕋 Mark

Style composition now uses `++` (after refactoring):
```lean
instance : Mul ℍ where
  mul x y := ℍ.mk (x.s ++ y.s) x.g
```
-/

def 𝕋style (w : Sty.Style) : FreeMonad.ℍ := ℍ.mk w (scale 1)
def myStyle : Sty.Style := {stroke_color := Color.mk 0 0 1, fill_color := Color.mk 1 1 0}

#html draw (𝕋style borderToBlue * 𝕋square)
#html draw (𝕋style borderToBlue * 𝕋style bigBorder * 𝕋square)

-- ### Envelopes and BoundingBox for 𝕋 Mark
def blueBorderSquare : FreeMonad.𝕋 Mark := (𝕋style borderToBlue * 𝕋style bigBorder * 𝕋square)

def bb_m₁ := bounding_box_𝕋 blueBorderSquare
def bb_m₂ := bounding_box_𝕋 twoCircles
#html draw blueBorderSquare (BoundingBox.toFrame bb_m₁)
#html draw twoCircles (BoundingBox.toFrame bb_m₂)

-- Envelope positioning for 𝕋 Mark
#html draw (twoCircles + envelope_position_marks twoCircles ![0,1] twoCircles)
#html draw (twoCircles → twoCircles ↑ twoCircles)
#html draw (twoCircles ↑ 𝕋square)

-- Positioning with spacing
def bb_m₃ := bounding_box_𝕋 (twoCircles ↑[0.5] twoCircles)
#html draw (twoCircles ↑[0.5] twoCircles) (BoundingBox.toFrame bb_m₃)

/-!
## 9. Creating Custom Marks

One of the most powerful features is creating new graphical objects.
The Mark system is defined as:

```lean
class MarkInterface (a : Type) where
  θ : a → Array Prim

structure Mark where
  {T : Type}
  [inst : MarkInterface T]
  val : T
```

To create a custom mark:
1. Define a structure for your mark's parameters
2. Implement `MarkInterface` with the rendering function `θ`
3. Optionally add `Coe` instance for automatic conversion to `Mark`
-/

-- Define the type
structure Sierpinski where
  n : Nat

instance : ToString Sierpinski where
  toString s := s!"Sierpinski {s.n}"

-- Implement MarkInterface
instance : MarkInterface Sierpinski where
  θ s := sierpinskiPrims s.n

def Sierpinski₃ : Sierpinski := {n := 3}

instance : Coe Sierpinski Mark where
  coe m := Mark.mk m

#html draw₁ Sierpinski₃

def TT : FreeMonad.ℍ := ℍ.mk {} (translate ![3,0])
#html draw (TT * (Sierpinski₃ : FreeMonad.𝕋 Mark))

/-!
## 10. Example: Regular Polygons
-/

structure RegularPolygon where
  center : Vec2 := ![0,0]
  sides : Nat
  size : Float
  h : sides >= 3 := by decide
  style : Sty.Style

instance : ToString RegularPolygon where
  toString p := s!"Regular Polygon with {p.sides} sides of size {p.size}"

-- Helper functions for generating polygon vertices
def findPointbyAngle (x : Float) : Vec2 :=
  ![Float.cos x, Float.sin x]

def create_list (n : ℕ) : Array ℕ :=
  Array.range n |>.map (λ x => x + 1)

def multiply_by_scalar (lst : Array ℕ) (scalar : Float) : Array Float :=
  lst.map (λ x => x.toFloat * scalar)

def regToPoly (p : RegularPolygon) : Array Vec2 :=
  let step : Float := (2 * π) / p.sides.toFloat
  let listn : Array ℕ := create_list p.sides
  let arr := multiply_by_scalar listn step
  Array.map findPointbyAngle arr

instance : MarkInterface RegularPolygon where
  θ h := new_polygon (regToPoly h) h.style

instance : Coe RegularPolygon Mark where
  coe m := Mark.mk m

def triangle : RegularPolygon := {center := ![0,0], sides := 3, size := 1, style := toBlue}
def triangleₘ : FreeMonad.𝕋 Mark := triangle
#check (triangle : FreeMonad.𝕋 Mark)
#html draw (𝕋rotate (π/4) * triangleₘ)
#html draw (𝕋style redBorder * (𝕋style bigBorder * triangleₘ))

/-!
## 11. Composing Custom Marks

We can create marks that use other marks as components.
-/

structure Arrow where
  p₁ : Vec2
  p₂ : Vec2
  tip : Mark
  style : Sty.Style

instance : ToString Arrow where
  toString _ := s!"Arrow"

def ArrowLine (α : Arrow) : Prim := new_line α.p₁ α.p₂ α.style

instance : MarkInterface Arrow where
  θ α :=
    let c₁ : Mark := ArrowLine α
    let c₂ : Mark := α.tip
    let m : FreeMonad.𝕋 Mark := envelope_position_marks c₁ (α.p₂ - α.p₁) c₂
    FreeMonad.flat m

instance : Coe Arrow Mark where
  coe m := Mark.mk m

def Arrow₁ : Arrow := {p₁ := ![0,0], p₂ := ![1,0], tip := triangle, style := bigBorder ++ borderToBlue}
#eval Arrow₁.p₂ - Arrow₁.p₁
#html draw₁ Arrow₁
