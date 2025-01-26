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

instance : HAdd (𝕋 Mark) (𝕋 Mark) (𝕋 Mark) where
  hAdd m1 m2 := 𝕋.comp m1 m2

instance  {α β : Type} [MarkInterface α] [MarkInterface β] [PrimInterface α] [PrimInterface β] [ToString α] [ToString β] :
  HAdd α β (𝕋 Mark) where
  hAdd p1 p2 := 𝕋.comp (𝕋.pure ⟨p1⟩) (𝕋.pure ⟨p2⟩)

instance  {α : Type} [MarkInterface α] [PrimInterface α] [ToString α] : HAdd α (𝕋 Mark) (𝕋 Mark) where
  hAdd p m := 𝕋.comp (𝕋.pure ⟨p⟩) m

instance  {β : Type} [MarkInterface β] [PrimInterface β] [ToString β] : HAdd (𝕋 Mark) β (𝕋 Mark) where
  hAdd m p := 𝕋.comp m (𝕋.pure ⟨p⟩)

end mark
