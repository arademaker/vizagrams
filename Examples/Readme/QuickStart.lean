import Vizagrams

open VizBackend
open ProofWidgets Svg
open Sty
-- Simple circle
def circle := new_circle 1 ![1,1] {fill_color := Color.mk 1 0 0}

#html draw circle default_frame

open LinearAlgebra
open FreeMonad
open GraphicalMark

def square := new_polygon #[![-1,-1], ![-1,1], ![1,1], ![1,-1]]
#html draw square default_frame
def rotated := rotate (π/4) * square
#html draw rotated default_frame
-- Scaling and styling

def styled := ℍ.mk {fill_color := Color.mk 1 0 0,
                    stroke_color := Color.mk 0 0 1,
                    stroke_width := Sty.StyleSize.px 4}
                   (scale 2)

#html draw (styled * (square: 𝕋 Mark)) default_frame

def blueCircle :=
    new_circle 1 ![0,0] {fill_color := Color.mk 0 0 1}

def redTriangle :=
    new_polygon #[![-0.85,-0.5],![0.85,-0.5],![0,1]]
                               {fill_color := Color.mk 1 0 0}
def greenSquare :=
    scale 0.5 *
    new_polygon #[![-1,-1], ![-1,1], ![1,1], ![1,-1]]
                {fill_color := Color.mk 0 1 0}

-- Combine all three shapes
def composition :=
    (blueCircle: 𝕋 Mark) +
    (redTriangle : 𝕋 Mark) +
    (greenSquare : 𝕋 Mark)

#html draw composition default_frame

def c₁ : 𝕋 Mark :=
  new_circle 1 ![0,0] {fill_color := Color.mk 1 0 0}

def c₂ : 𝕋 Mark :=
  new_polygon #[![0,-0.5], ![1,-0.5], ![1,0.5], ![0,0.5]]
                     {fill_color := Color.mk 0 0 1}

-- Place c₂ to the right of c₁ with gap of 1 unit
def layout := c₁ →[1] c₂

-- Vertical layout: c₁ above c₂
def vLayout := c₁ ↑[0.5] c₂

#html draw layout default_frame

-- Create a composed mark
def innerDiagram : 𝕋 Mark:= blueCircle →[1] redTriangle

-- Use it as part of a larger diagram
def outerCircle := new_circle 2.5 ![0,0]
  {fill_color := Color.mk 1 1 1,
   stroke_color := Color.mk 0 0 0,
   stroke_width := Sty.StyleSize.px 1}

def final := outerCircle + innerDiagram

#html draw final default_frame

#check rotate (π/4)           -- Rotate 45 degrees
#check scale 2                -- Uniform scaling by factor 2
#check translate ![1, 2]      -- Translate by vector
#check ℍ.mk {} (rotate (π/4))        -- Combined style and geometric transformation
