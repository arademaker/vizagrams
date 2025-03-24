import Vizagrams.Primitivesorg
import Vizagrams.BackendSVG
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Data.Svg

set_option autoImplicit true
set_default_scalar Float

namespace std

open Primitives
open backendsvg

/- Drawing a Circle -/

/- First, we can start by drawing a unit circle. -/
def circlePrim : Circle := Circle.mk 1 ⊞[0, 0]
#eval circlePrim -- Circle (r: 1.000000, c: ⊞[0.000000, 0.000000])

/- How to draw a single circle?
The `drawsvg` function:

def drawsvg (a : Array Prim) (fr : Frame := frame) : ProofWidgets.Html

takes an `Array` of primitives. However, a `Circle` alone is not an array.
We can still draw it by composing it with an empty array, either on the right or on the left.
-/
#html drawsvg (circlePrim ⊕ (#[] : Array Prim))
#html drawsvg ((#[] : Array Prim) ⊕ circlePrim)

/- We see that our addition operation `⊕` has #[] as a neutral element.
Now, let us explore other properties.
-/

/- The operation is **not commutative**, that is:
α + β ≠ β + α.

Below, we see that elements at the beginning of the array are drawn first
and can be overlaid by others.
-/
#html drawsvg ((Circle.mk 0.5 ⊞[0, 1]) ⊕ circlePrim)
#html drawsvg (circlePrim ⊕ (Circle.mk 0.5 ⊞[0, 1]))
