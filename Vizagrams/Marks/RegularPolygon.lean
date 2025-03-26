import Vizagrams.VizMark
import Vizagrams.VizBackend
import Vizagrams.VizPrim

open SciLean Scalar RealScalar
open GraphicalMark
open VizBackend
open GeometricPrimitive
open GraphicalPrimitive
open ProofWidgets Svg

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

def factorial : ℕ → ℕ
| 0 => 1
| (n+1) => (n+1) * factorial n

def sin_approx (x : Float) : Float :=
  (List.range 15).foldl (λ acc k => acc + ((-1)^k * x^(2 * k + 1)) / (factorial (2 * k + 1)).toFloat) 0

def cos_approx (x : Float) : Float :=
  (List.range 15).foldl (λ acc k => acc + ((-1)^k * x^(2 * k)) / (factorial (2 * k)).toFloat) 1

def findPointbyAngle (x : Float) : Float^[2] :=
  ⊞[cos_approx x , sin_approx x]

def create_list (n : ℕ) : Array ℕ :=
  Array.range n |>.map (λ x => x + 1)

def multiply_by_scalar (lst : Array ℕ) (scalar : Float) : Array Float :=
  lst.map (λ x => ↑x * scalar)

def regToPoly (p : RegularPolygon) : Array (Float^[2]) :=
  let passo : Float := ( 2 * 3.1415 ) / (p.sides)
  let listn : Array ℕ := create_list (p.sides)
  let Arr := multiply_by_scalar listn passo
  Array.map findPointbyAngle Arr

instance : MarkInterface RegularPolygon where
  θ h := NewPolygon (regToPoly h)

def triangle : RegularPolygon :=
  RegularPolygon.mk 3 5.0 (by decide)

def triangle_m := Mark.mk triangle
#eval (triangle_m : Array Prim)

#html drawsvg triangle_m
