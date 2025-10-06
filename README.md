# Vizagrams

[![Lean Action CI](https://github.com/arademaker/vizagrams/actions/workflows/lean_action_ci.yml/badge.svg)](https://github.com/arademaker/vizagrams/actions/workflows/lean_action_ci.yml)
[![Lean Version](https://img.shields.io/badge/Lean-4.20.0--rc5-blue)](https://lean-lang.org/)
[![Mathlib](https://img.shields.io/badge/Mathlib-✓-green)](https://github.com/leanprover-community/mathlib4)

Vizagrams is a visualization framework that integrates **diagramming**
and **data visualization**, with the goal of providing high
expressiveness with intuitive usability.

The framework implements a diagramming DSL together with a
visualization grammar, thus allowing users to create diagrams by
combining and transforming plots, as well as to create new
visualization specifications using diagram construction operations.

For the original Julia implementation, visit:

https://github.com/davibarreira/Vizagrams.jl

This Lean implementation is still under development.

## Project Structure

The Vizagrams library is organized into the following core modules:

### Core Modules

- **[LeannearAlgebra](Vizagrams/LeannearAlgebra.lean)** - Mathematical foundation for 2D geometric operations. Provides vector spaces (`Vec2`), affine transformations (`Mat2Vec2`), and geometric operations following Felix Klein's Erlanger Program.

- **[Geom](Vizagrams/Geom.lean)** - Geometric primitives with two representations:
  - `Geom`: Semantic representation (intuitive for users, e.g., circle defined by center and radius)
  - `CovGeom`: Covariant representation (points-based, ideal for applying transformations)

- **[Style](Vizagrams/Style.lean)** - Styling primitives for geometric objects. Defines `Style` structures with attributes like stroke/fill colors and widths, with composable styling via the `++` operator.

- **[Prim](Vizagrams/Prim.lean)** - Graphical primitives that combine geometry with style, forming the basic building blocks of diagrams.

- **[Mark](Vizagrams/Mark.lean)** - Higher-level graphical marks that can be composed and transformed to create complex visualizations.

- **[FreeMonad](Vizagrams/FreeMonad.lean)** - Free monad structure (`𝕋`) providing compositional semantics for graphical marks. Enables building complex diagrams through algebraic composition (`+`) and transformation (`*`) operators.

- **[Envelope](Vizagrams/Envelope.lean)** - Envelope functions and bounding box operations for layout. Provides precise positioning, alignment, and spatial composition using directional operators (`→`, `←`, `↑`, `↓`).

- **[VizBackend](Vizagrams/VizBackend.lean)** - Rendering backend that converts abstract diagram representations to SVG output using ProofWidgets.

- **[GraphicExpression](Vizagrams/GraphicExpression.lean)** - Expression types for graphic specifications.

- **[DataFrame](Vizagrams/DataFrame.lean)** - Data structure support for data-driven visualizations.

## Documentation and Examples

### Tutorials

Learn Vizagrams through hands-on tutorials in the [Test/Tutorials](Test/Tutorials/) directory:

- **[Basics.lean](Test/Tutorials/Basics.lean)** - Introduction to drawing simple diagrams, applying transformations, composition operations, and basic layout techniques.

- **[Custommarks.lean](Test/Tutorials/Custommarks.lean)** - Advanced tutorial on creating custom graphical marks by implementing the `MarkInterface` typeclass. Includes examples like creating custom arrow marks.

## Installation
To use Vizagrams in your Lean project, add it as a dependency in your lakefile.lean:

```
require vizagrams from git "https://github.com/arademaker/vizagrams"
```

## Usage Guide

### Basic Concepts

Vizagrams uses a compositional approach to building diagrams:

1. **Marks** - The basic graphical elements (circles, rectangles, polygons, etc.)
2. **Transformations** - Geometric (`ℍ`) and style transformations applied with `*`
3. **Composition** - Combining marks with `+`
4. **Layout** - Positioning marks with directional operators (`→`, `←`, `↑`, `↓`)

### Quick Start

#### 1. Creating Basic Shapes

Start by importing the necessary modules and creating simple shapes:

```lean
import Vizagrams
open ProofWidgets Svg

-- Simple circle
def circle := NewCircle 1 ![0,0] {fillColor := Color.mk 1 0 0}

#html draw circle defaultFrame
```

#### 2. Applying Transformations

Use the `*` operator to apply geometric and style transformations:

```lean
-- Rotation
def square := NewPolygon #[![-1,-1], ![-1,1], ![1,1], ![1,-1]]
def rotated := rotate (π/4) * square

-- Scaling and styling
def styled := ℍ.mk {fillColor := Color.mk 1 0 0,
                    strokeColor := Color.mk 0 0 1,
                    strokeWidth := Sty.StyleSize.px 4}
                   (scale 2) * square

#html draw rotated defaultFrame
```

#### 3. Composing Diagrams

Combine multiple marks using the `+` operator:

```lean
def blueCircle := NewCircle 1 ![0,0] {fillColor := Color.mk 0 0 1}
def redTriangle := NewPolygon #[![-0.85,-0.5],![0.85,-0.5],![0,1]]
                               {fillColor := Color.mk 1 0 0}
def greenSquare := scale 0.5 * NewPolygon #[![-1,-1], ![-1,1], ![1,1], ![1,-1]]
                                          {fillColor := Color.mk 0 1 0}

-- Combine all three shapes
def composition := blueCircle + redTriangle + greenSquare

#html draw composition defaultFrame
```

#### 4. Layout with Directional Operators

Use envelope-based positioning for precise layout:

```lean
def c₁ := NewCircle 1 ![0,0] {fillColor := Color.mk 1 0 0}
def c₂ := NewPolygon #[![0,-0.5], ![1,-0.5], ![1,0.5], ![0,0.5]]
                     {fillColor := Color.mk 0 0 1}

-- Place c₂ to the right of c₁ with gap of 1 unit
def layout := c₁ →[1] c₂

-- Vertical layout: c₁ above c₂
def vLayout := c₁ ↑[0.5] c₂

#html draw layout defaultFrame
```

#### 5. Advanced Example: Nested Composition

```lean
-- Create a composed mark
def innerDiagram := blueCircle →[1] redTriangle

-- Use it as part of a larger diagram
def outerCircle := NewCircle 2.5 ![0,0]
  {fillColor := Color.mk 1 1 1,
   strokeColor := Color.mk 0 0 0,
   strokeWidth := Sty.StyleSize.px 1}

def final := outerCircle + innerDiagram

#html draw final defaultFrame
```

### Key Operators

- **`+`** - Compose marks (overlay)
- **`*`** - Apply transformation to mark
- **`→[gap]`** - Position right with gap
- **`←[gap]`** - Position left with gap
- **`↑[gap]`** - Position above with gap
- **`↓[gap]`** - Position below with gap

### Common Transformations

```lean
rotate (π/4)           -- Rotate 45 degrees
scale 2                -- Uniform scaling by factor 2
translate ![x, y]      -- Translate by vector
ℍ.mk style geom        -- Combined style and geometric transformation
```
