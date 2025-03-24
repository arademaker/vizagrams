import Vizagrams.Markorg
import Vizagrams.FreeMonad

open Primitives
open mark
open FreeMonad


structure Head where
  size : Float
  smile : Float
deriving Repr

instance : ToString Head where
  toString h := "Head (size: " ++ toString h.size ++ ", smile: " ++ toString h.smile ++ ")"

instance : MarkInterface Head where
  θ h :=
    let eyes := (Circle.mk 0.3 ⊞[-0.8,1]) ⊕ (Circle.mk 0.3 ⊞[0.8,1])
    let smile := Line.mk (⊞[-1,-0.5], ⊞[1,-0.5])
    let head := Circle.mk (2*h.size) ⊞[0,0]
    head ⊕ eyes ⊕ smile
