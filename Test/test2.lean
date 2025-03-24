import Vizagrams.head
import Vizagrams.FreeMonad
import Vizagrams.Markorg
import Vizagrams.BackendSVG
import Vizagrams.Primitivesorg

open backendsvg
open FreeMonad
open mark
open Primitives

def Head.o : Head := Head.mk 1.0 0.0
private def x : 𝕋 Mark := pure ⟨Head.o⟩

def Circle.o : Circle := Circle.mk 1 ⊞[0,0]
def Line.o : Line := Line.mk (⊞[0,0], ⊞[1,1])

#eval algθ (Mark.θ <$> x)
#html drawsvg (algθ (Mark.θ <$> x))

open ProofWidgets Svg in
private def frame2 : Frame where
  xmin   := -5
  ymin   := -5
  xSize  := 10
  width  := 400
  height := 400

structure Adam where
  head : Head
  height : Float
instance : ToString Adam where
  toString h := "Adam (head: " ++ toString h.head ++ ", height: " ++ toString h.height ++ ")"
def Adam.o (head : Head := Head.o) : Adam := Adam.mk head (7 * head.size)

def Adam.ζ (adam : Adam) : 𝕋 Mark :=
  let head := adam.head
  let body := Line.mk (⊞[0.,0.],⊞[0,-adam.height])
  let diag : 𝕋 Mark := 𝕋.comp (𝕋.pure ⟨body⟩) (𝕋.pure ⟨head⟩)
  diag
instance : MarkInterface Adam where
  θ adam := algθ (Mark.θ <$> Adam.ζ adam)

#html Mark.draw (.pure ⟨Adam.o⟩ : 𝕋 Mark) frame2

def w :𝕋 Mark  := 𝕋.comp (𝕋.pure ⟨Adam.o⟩) (𝕋.comp (𝕋.pure ⟨Circle.mk 2.0 ⊞[2,1]⟩) (𝕋.pure ⟨Adam.o⟩))

#html Mark.draw w frame2

private def y :𝕋 Mark  := 𝕋.comp (𝕋.pure ⟨Head.o⟩) (𝕋.comp (𝕋.pure ⟨Circle.o⟩) (𝕋.pure ⟨Line.o⟩))
#eval algθ (Mark.θ <$> y)

private def z :𝕋 Mark  := 𝕋.comp (𝕋.pure ⟨Circle.o⟩) (𝕋.comp (𝕋.pure ⟨Circle.o⟩) (𝕋.pure ⟨Line.o⟩))
#eval algθ (Mark.θ <$> z)

#check x + z
#check Circle.o + x
#html Mark.draw (x+z) frame2
#html Mark.draw ((Circle.mk 2.0 ⊞[3,0]) + x) frame2
#html Mark.draw ((x + Circle.mk 2.0 ⊞[3,0]) + Line.o) frame2
#html Mark.draw ((x + Circle.mk 2.0 ⊞[3,0]) + Line.o)
