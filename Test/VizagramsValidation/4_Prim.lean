import Vizagrams.Prim

open LinearAlgebra
open GeometricPrimitive
open Sty ProofWidgets.Svg
open GraphicalPrimitive

def g₁ : Geom := .circle 1 ![0,0]
def s₁ : Style := {fillColor := Color.mk 0 1 0}

def p₁ : Prim := {geom := g₁ , style := s₁ }

#eval p₁

#check (p₁ : Array Prim)
#check (p₁ : Geom)

def f : Mat2Vec2 := {A := !![2 , 0 ; 0 , 2] , b := ![1 ,0]}

#eval f * p₁

def p₂ : Prim := line ![0 , 0] ![1 , 1]

#eval p₁ ⊕ p₂
#check (p₁ ⊕ p₂)

def a := (p₁ ⊕ p₂)

#eval f * a

#eval (p₁ ⊕ p₂)

#eval (p₂ ⊕ p₁)

#eval p₁ ⊕ p₂ ⊕ a
