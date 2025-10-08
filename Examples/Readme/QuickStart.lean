import Vizagrams

open VizBackend
open ProofWidgets Svg
open Sty
-- Simple circle
def circle := NewCircle 1 ![1,1] {fillColor := Color.mk 1 0 0}

#html draw circle defaultFrame

open LinearAlgebra
open FreeMonad
open GraphicalMark

def square := NewPolygon #[![-1,-1], ![-1,1], ![1,1], ![1,-1]]
#html draw square defaultFrame
def rotated := rotate (π/4) * square
#html draw rotated defaultFrame
-- Scaling and styling

def styled := ℍ.mk {fillColor := Color.mk 1 0 0,
                    strokeColor := Color.mk 0 0 1,
                    strokeWidth := Sty.StyleSize.px 4}
                   (scale 2)

#html draw (styled * (square: 𝕋 Mark)) defaultFrame

def blueCircle :=
    NewCircle 1 ![0,0] {fillColor := Color.mk 0 0 1}

def redTriangle :=
    NewPolygon #[![-0.85,-0.5],![0.85,-0.5],![0,1]]
                               {fillColor := Color.mk 1 0 0}
def greenSquare :=
    scale 0.5 *
    NewPolygon #[![-1,-1], ![-1,1], ![1,1], ![1,-1]]
                {fillColor := Color.mk 0 1 0}

-- Combine all three shapes
def composition :=
    (blueCircle: 𝕋 Mark) +
    (redTriangle : 𝕋 Mark) +
    (greenSquare : 𝕋 Mark)

#html draw composition defaultFrame

def c₁ : 𝕋 Mark :=
  NewCircle 1 ![0,0] {fillColor := Color.mk 1 0 0}

def c₂ : 𝕋 Mark :=
  NewPolygon #[![0,-0.5], ![1,-0.5], ![1,0.5], ![0,0.5]]
                     {fillColor := Color.mk 0 0 1}

-- Place c₂ to the right of c₁ with gap of 1 unit
def layout := c₁ →[1] c₂

-- Vertical layout: c₁ above c₂
def vLayout := c₁ ↑[0.5] c₂

#html draw layout defaultFrame

-- Create a composed mark
def innerDiagram : 𝕋 Mark:= blueCircle →[1] redTriangle

-- Use it as part of a larger diagram
def outerCircle := NewCircle 2.5 ![0,0]
  {fillColor := Color.mk 1 1 1,
   strokeColor := Color.mk 0 0 0,
   strokeWidth := Sty.StyleSize.px 1}

def final := outerCircle + innerDiagram

#html draw final defaultFrame

#check rotate (π/4)           -- Rotate 45 degrees
#check scale 2                -- Uniform scaling by factor 2
#check translate ![1, 2]      -- Translate by vector
#check ℍ.mk {} (rotate (π/4))        -- Combined style and geometric transformation
