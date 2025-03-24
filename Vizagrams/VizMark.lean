import Vizagrams.VizPrim
import Vizagrams.FreeMonad

open FreeMonad
open GraphicalPrimitive
set_option autoImplicit true

namespace GraphicalMark

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

instance : MarkInterface Prim where
  θ p := #[p]

def Mark.flat (t : 𝕋 Mark) : Array Prim := algθ ((𝕋.map Mark.θ) t)

instance : Coe Mark (Array Prim) where
  coe m := m.θ

instance : HAdd (𝕋 Mark) (𝕋 Mark) (𝕋 Mark) where
  hAdd m1 m2 := 𝕋.comp m1 m2
