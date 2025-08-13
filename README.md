# Vizagrams 

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

## Package Features

## Quick Start

Let's start by building some primitives.
We can start by defining a circle and a square, one beside the other.

```lean
def d₁ : GraphicalPrimitive.Prim := {
  geom := GeometricPrimitive.Geom.circle 1 ![0,0]
  style := {fillColor := ProofWidgets.Svg.Color.mk 0 0 0} 
}

def d₂ : GraphicalPrimitive.Prim := {
  geom := GeometricPrimitive.Geom.rect ![0,0] 1 1
  style := {fillColor := ProofWidgets.Svg.Color.mk 0 0 0}
}

def fr : ProofWidgets.Svg.Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 10
  width  := 400
  height := 400

def d₃ := translate ![2,2] * d₂ 

#html VizBackend.draw 
  (d₁ + ( translate ![2,0.5] * d₂ : GraphicalPrimitive.Prim) ) 
  fr
```
-- image

This is quite verbose, but it's more explicit about what's happening. You can achieve the same result like this:

```lean
open VizBackend

def c₁ := NewCircle 1 ![0,0]
def c₂ := NewPolygon #[![0,0], ![1,0], ![1,1], ![0,1]]

#html draw (c₁ →[1] c₂ ) fr

```
-- image