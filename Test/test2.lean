import Vizagrams.head
import Vizagrams.FreeMonad
import Vizagrams.Markorg
import Vizagrams.BackendSVG

open backendsvg
open FreeMonad
open mark

def Head.o : Head := Head.mk 1.0 0.0
private def x : 𝕋 Mark := pure ⟨Head.o⟩

#eval algθ (Mark.θ <$> x)
#html drawsvg (algθ (Mark.θ <$> x))
