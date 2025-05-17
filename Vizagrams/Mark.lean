import Vizagrams.Prim

open GeometricPrimitive
open GraphicalPrimitive
set_option autoImplicit true

namespace GraphicalMark
universe u
class MarkInterface (a : Type u) where
  θ : a -> Array Prim

structure Mark where
  {T : Type u}
  [inst : MarkInterface T]
  --[strg : ToString T]
  val : T

def Mark.θ : Mark → Array Prim := fun m => m.inst.θ m.val

instance : ToString Mark where
  toString p := s!"MArk" --@ToString.toString p.T p.strg p.val

instance : MarkInterface Prim where
  θ p := #[p]

instance : Coe Mark (Array Prim) where
  coe m := m.θ

instance : Coe Prim Mark where
  coe p := { val := p }

instance  : HPlus  Prim (Mark) where
  hPlus p1 p2 := #[p1] ++ p2
instance  : HPlus  (Mark) Prim where
  hPlus p1 p2 := p1 ++ #[p2]
instance  : HPlus (Mark) (Mark) where
  hPlus p1 p2 := p1 ++ p2
instance  : HPlus (Array Prim) (Mark) where
  hPlus p1 p2 := p1 ++ p2

instance : HMul Mat2Vec2 Mark (Array Prim) where
  hMul g M  := g * M.θ

instance : HMul Mark Mat2Vec2 (Array Prim) where
  hMul M g := g * M.θ

end GraphicalMark
