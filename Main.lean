import Vizagrams

def Vizagrams := "Vizagrams"

#eval Vizagrams


open ProofWidgets Svg in
private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400
private def x := @ProofWidgets.Svg.line frame (0.,0.) (1.,0.)

-- Desenhar circle
def circle : Primitives.Circle := Primitives.Circle.mk 1 ⊞[0,0]
#eval circle
