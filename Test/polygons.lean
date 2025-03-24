import Vizagrams.Primitivesorg
import Vizagrams.BackendSVG

set_option autoImplicit true
set_default_scalar Float

namespace polygons

open Primitives
open backendsvg

def triangle.o : Polygon := ⟨ #[⊞[0,0.6] , ⊞[0,1] , ⊞[0.5,0.6]] ⟩

open ProofWidgets Svg in
private def frame : Frame where
  xmin   := -3
  ymin   := -3
  xSize  := 5
  width  := 500
  height := 500
private def x := @ProofWidgets.Svg.line frame (0.,0.) (1.,0.)

#html drawsvg (triangle.o ⊕ (#[] : Array Prim))

def square.o : Polygon := ⟨ #[⊞[0,0] , ⊞[0,0.5] , ⊞[0.5,0.5], ⊞[0.5,0]] ⟩
#html drawsvg (square.o ⊕ (#[] : Array Prim))

#html drawsvg (square.o ⊕ triangle.o)
