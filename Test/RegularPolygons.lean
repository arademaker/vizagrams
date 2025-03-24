import Vizagrams.head
import Vizagrams.FreeMonad
import Vizagrams.Markorg
import Vizagrams.BackendSVG
import Vizagrams.Primitivesorg
import SciLean

open SciLean Scalar RealScalar
open backendsvg
open FreeMonad
open mark
open Primitives

set_option autoImplicit true
set_default_scalar Float

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

#eval findPointbyAngle π

def create_list (n : ℕ) : Array ℕ :=
  Array.range n |>.map (λ x => x + 1)

def multiply_by_scalar (lst : Array ℕ) (scalar : Float) : Array Float :=
  lst.map (λ x => ↑x * scalar)

#eval create_list 4
#eval multiply_by_scalar (create_list 4) (2*3.14/4)
#eval Array.map findPointbyAngle (multiply_by_scalar (create_list 4) (2*π/4))

def regToPoly (p : RegularPolygon) : Array (Float^[2]) :=
  let passo : Float := ( 2 * π ) / (p.sides)
  let listn : Array ℕ := create_list (p.sides)
  let Arr := multiply_by_scalar listn passo
  Array.map findPointbyAngle Arr

def RegPoly.o : RegularPolygon := ⟨ 10 , 1 , by simp⟩
#eval RegPoly.o
def Poly : Polygon := ⟨ regToPoly RegPoly.o ⟩

def Pentagon : RegularPolygon := ⟨ 5, 1 , by simp ⟩
def hexagon : RegularPolygon := ⟨ 6 , 1 , by simp ⟩
def pent.o : Polygon := ⟨ regToPoly Pentagon ⟩
def hexa.o : Polygon := ⟨ regToPoly hexagon ⟩

#html drawsvg (pent.o ⊕ hexa.o)
