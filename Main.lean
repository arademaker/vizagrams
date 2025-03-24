import Vizagrams

def Vizagrams := "Vizagrams"

open Primitives
open backendsvg
open mark
open FreeMonad

-- Desenhando um circle:
def circlePrim : Circle := Circle.mk 1 ⊞[0, 0]
#eval circlePrim -- Circle (r: 1.000000, c: ⊞[0.000000, 0.000000])

#html drawsvg (circlePrim ⊕ (#[] : Array Prim))

-- Compor circles
#html drawsvg ((Circle.mk 0.5 ⊞[0, 1]) ⊕ circlePrim)

-- Marca Head
def Head.o : Head := Head.mk 1.0 0.0
private def x : 𝕋 Mark := pure ⟨Head.o⟩

#eval algθ (Mark.θ <$> x)
#html drawsvg (algθ (Mark.θ <$> x))

#eval Vizagrams
