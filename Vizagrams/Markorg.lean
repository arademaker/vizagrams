import Vizagrams.Primitivesorg
import Vizagrams.FreeMonad
import Vizagrams.BackendSVG

open Primitives
open FreeMonad
open backendsvg

namespace mark

class MarkInterface (a : Type) where
  θ : a -> Array Prim

structure Mark where
  {T : Type}
  [inst : MarkInterface T]
  [strg : ToString T]
  val : T

def Mark.θ : Mark → Array Prim := fun m => m.inst.θ m.val

instance : ToString Mark where
  toString p := @ToString.toString p.T p.strg p.val

instance : MarkInterface Circle where
  θ p := #[prim p]

instance : MarkInterface Line where
  θ p := #[prim p]

-- def Mark.nil :𝕋 Mark  := 𝕋.pure ⟨Circle.o⟩

def algθ : 𝕋 (Array Prim) → Array Prim
  | 𝕋.pure x => x
  | 𝕋.comp x y => (algθ x) ⊕ (algθ y)
  | 𝕋.act h x => algθ x

def Mark.flat (t : 𝕋 Mark) : Array Prim := algθ (Mark.θ <$> t)

open ProofWidgets Svg in
def Mark.draw (t : 𝕋 Mark) (fr : Frame := frame) : ProofWidgets.Html := drawsvg (Mark.flat t) fr

end mark
