/-
Copyright (c) 2025 Henrique Borges. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrique Borges
-/
import Mathlib.Analysis.InnerProductSpace.Basic
import Mathlib.Data.Matrix.auto
import Mathlib.Data.Matrix.Notation
import Mathlib.Analysis.Normed.Group.Basic
import Mathlib.LinearAlgebra.AffineSpace.AffineMap
import Mathlib.Data.Matrix.Basic
import Mathlib.Data.Matrix.Notation

/-!
# Linear Algebra Foundations for Vizagrams
This module defines the basic linear algebra types and operations used throughout the Vizagrams
library. It establishes the `Vec2` type for 2D vectors over `Float`, provides standard typeclass
instances for vector space operations, and defines core geometric transformations like rotation,
scaling, and translation using `Mathlib`'s `LinearMap` and `AffineMap`.
-/
namespace LinearAlgebra

open Matrix Fin

def π : Float := 3.141592653589793 -- Aproximação para π em Float

/-# Fin
Fin n representa o conjunto `Iₙ` ou `[n]`, isto é, os Naturais menores que n
podemos usar `Fin n` para indexar vetores com n posições
`Fin 2 → ℝ²` é o vetor de duas entradas reais
-/
abbrev Vec2 := Fin 2 → Float

def e₁ : Vec2 := ![1 ,0]
def e₂ : Vec2 := ![0 ,1]

instance : Inner Float Vec2 where
  inner v₁ v₂ := v₁ 0 * v₂ 0 + v₁ 1 * v₂ 1

notation "⟪" x "," y "⟫" => inner Float x y

def normSquare (v : Vec2) : Float := ⟪ v , v ⟫
notation:max "‖" v:max "‖²" => normSquare v

def norm (v : Vec2) : Float :=
  Float.sqrt (⟪v, v⟫)

notation:max "‖" v:max "‖" => norm v

def normalize (v : Vec2) : Vec2 :=
  let n := ‖ v ‖ ;
  if n == 0 then v else fun i => v i / n

def distance (v₁ v₂ : Vec2) : Float := ‖ (v₁ - v₂) ‖

notation "d("x","y")" => distance x y

def angleBetween (v₁ v₂ : Vec2) : Float := Float.acos (⟪v₁, v₂⟫ / (‖v₁‖ * ‖v₂‖))

def projection (v₁ v₂ : Vec2) : Vec2 := (⟪v₁, v₂⟫ / ‖v₂‖²) • v₂

def perpendicular (v : Vec2) : Vec2 := ![- (v 1), v 0]

/-# Mat2
`Defs.lean:` (Mathlib.Data.Matrix)
def Matrix (m : Type u) (n : Type u') (α : Type v) : Type max u u' v :=
  m → n → α

uma matriz é uma função que, dado um índice de linha `m` e um de coluna `n`,
retorna o elemento daquela posição
-/
-- Usar `!![ ; ]` vem de Matrix.Notation
abbrev Mat2 := Matrix (Fin 2) (Fin 2) Float

instance : One Mat2 where
  one := !![1 , 0 ; 0 , 1]

def matMul (A B : Mat2) : Mat2 :=
  fun i j => A i 0 * B 0 j + A i 1 * B 1 j

infixl:70 "∘ₘ" => matMul

def mulVec (A : Mat2) (v : Vec2) : Vec2 :=
  fun i => (A i 0) * v 0 + (A i 1) * v 1

infixl: 65 "@" => mulVec
-- Aqui definimos um Mat2Vec2, isto é, um tipo para representar x ↦ Ax + B
structure Mat2Vec2 where
  A : Mat2
  b : Vec2
deriving Repr

class AffineMapLike (G : Type) (V : Type) where
  eval : G → V → V
  compose : G → G → G

instance : AffineMapLike Mat2Vec2 Vec2 where
  eval f x := mulVec f.A x + f.b
  compose f g := { A := f.A ∘ₘ g.A , b := (f.A @ g.b) + f.b }

infixl:75 " ▷ " => AffineMapLike.eval
infixl:80 " ∘ₐ " => AffineMapLike.compose

def nullVec2 := ![0.0 ,0.0]

/-- Creates a translation transformation. -/
def translate (t : Vec2) : Mat2Vec2 :=
  { A := 1, b := t }

/-- Creates a uniform scaling transformation. -/
def scale (s : Float) : Mat2Vec2 :=
  { A := !![s, 0.0; 0.0, s], b := nullVec2 }

/-- Creates a rotation transformation around the origin. -/
def rotate (θ : Float) : Mat2Vec2 :=
  let c := Float.cos θ
  let s := Float.sin θ
  { A := !![c, -s; s, c], b := nullVec2 }

end LinearAlgebra
/-
def nullVec2 : Vec2 := ![0,0]

def rotateVec (v : Vec2) (θ : Float) : Vec2 :=
  let cosθ := Float.cos θ
  let sinθ := Float.sin θ
  ![cosθ * v 0 - sinθ * v 1, sinθ * v 0 + cosθ * v 1]

def pointOnEllipse (θ rx ry : Float) : Vec2 :=
  ![rx * Float.cos θ, ry * Float.sin θ]

def atan2pi (v : Vec2) : Float :=
  Float.atan2 (v 1) (v 0)


/- # Instanciando AffineMapClass para Mat2Vec2
Seja f : V² → V² uma trasformação linear tal que f(x) := Ax + b
portanto instanciamos o campo `eval f x` como `Ax + b`
Agora seja g : V² → V² tal que g(x) := Cx + d, temos que
(f ∘ g)(x) = f(g(x)) = f( Cx + d ) = A ( Cx + d ) + b = (A·C)x + (A · d + b)
-/


infixr:70 " ⬝ " => AffineMapLike.eval (G := Mat2Vec2) (V := Vec2)
infixr:70 " ∘ " => AffineMapLike.compose (G := Mat2Vec2) (V := Vec2)


def I2 : Mat2 := !![1, 0; 0, 1]

def translate (t : Vec2) : Mat2Vec2 :=
  { A := I2, b := t }

def scale (s : Float) : Mat2Vec2 :=
  { A := !![s, 0; 0, s], b := ![0.0, 0.0] }

def rotate (θ : Float) : Mat2Vec2 :=
  let c := Float.cos θ
  let s := Float.sin θ
  { A := !![c, -s; s, c], b := ![0.0, 0.0] }

def getCoordinates (v : Vec2) : String :=
  s!"{v 0} {v 1}"

-- Testes
private def v : Vec2 := ![1,1]
#eval v + v
#eval 2.0 • v
private def v₁ : Vec2 := ![1.0, 2.0]
private def v₂ : Vec2 := ![3.0, 4.0]
#eval (inner Float v₁ v₂)
#eval ⟪ v₁ , v₂ ⟫
#eval norm v
#eval (‖ v ‖)
-/
