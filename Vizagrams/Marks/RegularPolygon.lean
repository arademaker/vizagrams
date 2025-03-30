import Vizagrams.VizMark
import Vizagrams.VizBackend
import Vizagrams.VizPrim

open SciLean Scalar RealScalar
open GraphicalMark
open VizBackend
open GeometricPrimitive
open GraphicalPrimitive
open ProofWidgets Svg

set_option diagnostics true
set_default_scalar Float
/-
structure RegularPolygon where
 n : Float -- Número de lados
 c : Float^[2] -- Ponto central
 r : Float --
 α : Float -- Ângulo rotação


def create_list (n : ℕ) : Array ℕ :=
  Array.range n |>.map (λ x => x + 1)

def multiply_by_scalar (lst : Array ℕ) (scalar : Float) : Array Float :=
  lst.map (λ x => ↑x * scalar)

instance : MarkInterface RegularPolygon where
  θ p :=
    let passo : Float := ( 2 * π ) / (p.n)
    let listn : Array ℕ := create_list (p.n)
    let Arr := multiply_by_scalar listn passo
    Array.map findPointbyAngle Arr
-/
structure RegularPolygon where
  sides : Nat
  size : Float
  h : sides ≥ 3 -- Todo polígono tem ao menos 3 lados

instance : ToString RegularPolygon where
  toString p := s!" Regular Polygon : ( {p.sides} sides , size {p.size} )"

def findPointbyAngle (x : Float) : Float^[2] :=
  ⊞[Float.cos x , Float.sin x]

def create_list (n : ℕ) : Array ℕ :=
  Array.range n |>.map (λ x => x + 1)

def multiply_by_scalar (lst : Array ℕ) (scalar : Float) : Array Float :=
  lst.map (λ x => ↑x * scalar)

def regToPoly (p : RegularPolygon) : Array (Float^[2]) :=
  let passo : Float := ( 2 * π  ) / (p.sides)
  let listn : Array ℕ := create_list (p.sides)
  let Arr := multiply_by_scalar listn passo
  Array.map findPointbyAngle Arr

instance : MarkInterface RegularPolygon where
  θ h := NewPolygon (regToPoly h)

instance : Coe RegularPolygon Mark where
  coe m := Mark.mk m

def triangle : RegularPolygon :=
  RegularPolygon.mk 3 1.0 (by decide)

#check (triangle : Mark)

def triangle_m := Mark.mk triangle
#eval (triangle_m : Array Prim)

#html drawsvg triangle_m

def styleLine : Sty.Style := {strokeWidth := Sty.StyleSize.px 10 }
def MyLine := NewLine ⊞[0.0,0.0] ⊞[1.0,0.0]
#check styleLine * MyLine
#eval styleLine * MyLine
def myline := styleLine * MyLine
def translationrigth := GeometricTransformation.G.translate ⊞[1.5,0]
def myarroe := translationrigth * triangle_m

def arrow := (styleLine * MyLine) ⊕ (translationrigth * triangle_m)
#html drawsvg ((styleLine * MyLine) ⊕ (translationrigth * triangle_m))
