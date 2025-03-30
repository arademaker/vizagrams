import Vizagrams.VizPrim
import Vizagrams.FreeMonad
import Vizagrams.Transformations

open GeometricTransformation
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

instance  : HPlus  Prim (Mark) where
  hPlus p1 p2 := #[p1] ++ p2
instance  : HPlus  (Mark) Prim where
  hPlus p1 p2 := p1 ++ #[p2]
instance  : HPlus  (Mark) (Mark) where
  hPlus p1 p2 := p1 ++ p2

instance : HAdd (𝕋 Mark) (𝕋 Mark) (𝕋 Mark) where
  hAdd m1 m2 := 𝕋.comp m1 m2

instance : HMul G Mark (Array Prim) where
  hMul g M  := g * M.θ

instance : HMul Mark G  (Array Prim) where
  hMul M g := g * M.θ
