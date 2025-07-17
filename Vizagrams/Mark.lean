import Vizagrams.Prim

open GeometricPrimitive
open GraphicalPrimitive
set_option autoImplicit true

namespace GraphicalMark

-- A Type can be transform in Array Prim means that this Type can be darw by
-- α → Array Prim ⟶ Drawing
class MarkInterface (a : Type u) where
  θ : a -> Array Prim
universe u

/- A Mark is a Type that can be draw, this is the same of a Type that implements a MarkInterface
Instance -/
structure Mark where
  {T : Type u}
  [inst : MarkInterface T]
  val : T

/- Here, we define a Mark.ulift to allow us combine elements of Mark Type that living in
differents Type-Universes -/
def Mark.ulift (x : Mark.{u}) : Mark.{max u v} where
  T := ULift x.T
  val := ULift.up x.val
  inst := {
    θ := fun (v : ULift x.T) => x.inst.θ (ULift.down v)
  }

def Mark.θ : Mark → Array Prim := fun m => m.inst.θ m.val

-- Some Types that implement a MarkInterface
inductive Nil : Type
  | mk : Nil

-- Nil is similar to Unit, we can define an object like ∅ a exist a func f(∅) = #[]
instance : MarkInterface Nil where
  θ _ := #[]

instance : MarkInterface Unit where
  θ _ := #[]

-- A Prim cam be transformed in a Array Prim
instance : MarkInterface Prim where
  θ p := #[p]

-- Coersions
--We can create Coe instances of Mark → Array Prim because a Mark implements θ : Mark → Array Prim
instance : Coe Mark (Array Prim) where
  coe m := m.θ

-- A Prim can be interpreted as a Mark because a Prim "is the primitive draw"
instance : Coe Prim Mark where
  coe p := { val := p }

instance : Coe Nil Mark where
  coe m := Mark.mk m

instance : Coe Unit Mark where
  coe m := Mark.mk m

-- HPlus Instances
instance  : HPlus  Prim (Mark) where
  hPlus p1 p2 := #[p1] ++ p2
instance  : HPlus  (Mark) Prim where
  hPlus p1 p2 := p1 ++ #[p2]

instance  : HPlus (Array Prim) (Mark) where
  hPlus p1 p2 := p1 ++ p2

-- Sum of Marks
instance  : HPlus (Mark) (Mark) where
  hPlus p1 p2 := p1 ++ p2

instance : HMul Mat2Vec2 Mark (Array Prim) where
  hMul g M  := g * M.θ

instance : HMul Mark Mat2Vec2 (Array Prim) where
  hMul M g := g * M.θ


end GraphicalMark
