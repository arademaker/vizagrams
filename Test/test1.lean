import Vizagrams.Primitivesorg
import Vizagrams.BackendSVG

set_option autoImplicit true
set_default_scalar Float

namespace test1

open Primitives
open backendsvg
def Circle.o : Circle := Circle.mk 1 ⊞[0,0]
def Line.o : Line := Line.mk (⊞[0,0], ⊞[1,1])

#eval Circle.o
#eval Line.o

open ProofWidgets Svg in
private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400
private def x := @ProofWidgets.Svg.line frame (0.,0.) (1.,0.)


#check (PrimInterface.draw)
def d := PrimInterface.draw Circle.o frame

#check d
#html d.toHtml

#eval Prim.mk Circle.o
#eval prim Line.o
def fooo : Array Prim := #[⟨Circle.o⟩, ⟨Line.o⟩]

-- def foo : List Prim := [prim Circle.o, prim Line.o]
def foo : Array Prim := #[prim Circle.o, prim Line.o]

open ProofWidgets Svg in
private def svg : Svg frame :=
  { elements := Array.map (λx => Prim.draw x frame) foo}

#html svg.toHtml
#check svg.toHtml

#eval Circle.o ⊕ Circle.o

#eval Circle.o ⊕ Circle.o ⊕ Line.o
#html drawsvg (Circle.o ⊕ (Circle.mk 0.5 ⊞[0,1]) ⊕ Line.o)

open ProofWidgets Svg in
private def frame2 : Frame where
  xmin   := -5
  ymin   := -5
  xSize  := 10
  width  := 400
  height := 400
#html drawsvg (Circle.o ⊕ (Circle.mk 0.5 ⊞[1,1]) ⊕ Line.o) frame2
def eyes := (Circle.mk 0.3 ⊞[-0.8,1]) ⊕ (Circle.mk 0.3 ⊞[0.8,1])
#html drawsvg (Circle.mk 2.0 ⊞[0,0] ⊕ eyes ⊕ Line.mk (⊞[-1,-0.5], ⊞[1,-0.5])) frame2

#eval (#[] : Array Prim)
