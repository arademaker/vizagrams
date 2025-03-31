import Vizagrams.VizPrim
import Vizagrams.VizBackend
import Vizagrams.VizMark
import Vizagrams.Marks.RegularPolygon

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open GeometricTransformation
open SciLean Scalar RealScalar

set_default_scalar Float
-- Drawing a Circle
def exCircle : Mark := NewCircle  1 ⊞[0,0]

#html drawsvg exCircle

def exSquare : Prim := NewPolygon #[⊞[-0.5 , -0.5] ,⊞[0.5,-0.5], ⊞[0.5,0.5],⊞[-0.5,0.5]]

#html drawsvg exSquare

def rotate₄₅ := G.rotate (π/4)

def exSquareRotate := rotate₄₅ * exSquare
#eval exSquareRotate

#html drawsvg exSquareRotate

-- RigthOption
def exStyle : Sty.Style := {strokeColor := Color.mk 0 0 1 , fillColor := Color.mk 1 1 0}

#html drawsvg (exStyle * exSquare)

def exSquareStyle : Prim := NewPolygon #[⊞[-0.5 , -0.5] ,⊞[0.5,-0.5], ⊞[0.5,0.5],⊞[-0.5,0.5]]
{strokeColor := Color.mk 0 0 1 , fillColor := Color.mk 1 0 0}

#html drawsvg exSquareStyle
#html drawsvg ( rotate₄₅ * exSquareStyle)

def exCircle₁ : Mark := NewCircle 1 ⊞[0,0] {fillColor := Color.mk 0 0 1}
def exTriangle₁ : Mark := RegularPolygon.mk 3 1 (by decide) {fillColor := Color.mk 1 0 0}
def exSquare₁ : Mark := NewPolygon #[⊞[-0.5 , -0.5] ,⊞[0.5,-0.5], ⊞[0.5,0.5],⊞[-0.5,0.5]]
{fillColor := Color.mk 0 1 0}

#html drawsvg ( (exCircle₁ ⊕ exTriangle₁) ⊕ exSquare₁)

def fillblack : Sty.Style := {fillColor := Color.mk 1 1 0}
def exCircle₂ : Mark := NewCircle 1 ⊞[0,0]
def exTriangle₂ : Mark := RegularPolygon.mk 3 1 (by decide) Sty.styledefout
def exSquare₂ : Mark := NewPolygon #[⊞[-1, -1] ,⊞[1,-1], ⊞[1,1],⊞[-1,1]]

#html drawsvg ( ((G.translate ⊞[3,0])* exCircle) ⊕ ((G.translate ⊞[0,2]) * exTriangle₂) ⊕ ((G.rotate (π/10)) *exSquare₂))
