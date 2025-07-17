
import Vizagrams.LeannearAlgebra

-- Developing Geom and Prim
inductive Geom where
  | circle (r : Float) (c : Vec2)

structure Prim where
  g : Geom
  s : String

-- The same of previous Example, but with ours restrictions of Mark
class MarkInterface (a : Type u) where
  θ : a → Array Prim

-- This is like the Node
structure Mark where
  {T : Type u}
  [inst : MarkInterface T]
  val : T

def Mark.θ : Mark → Array Prim := fun m => m.inst.θ m.val

-- This is like the TreeStructure
inductive 𝕋 (α : Type u) where
  | pure : α → 𝕋 α
  | comp : 𝕋 α → 𝕋 α → 𝕋 α

-- Some instances to Add Marks and 𝕋 Marks
instance : HAdd (𝕋 Mark) (𝕋 Mark) (𝕋 Mark) where
  hAdd m1 m2 := 𝕋.comp m1 m2

instance : HAdd Mark (𝕋 Mark) (𝕋 Mark) where
  hAdd m t := 𝕋.comp (𝕋.pure m) t

instance : HAdd (𝕋 Mark) Mark (𝕋 Mark) where
  hAdd t m := 𝕋.comp t (𝕋.pure m)

instance : HAdd Mark Mark (𝕋 Mark) where
  hAdd t m := 𝕋.comp (𝕋.pure t) (𝕋.pure m)

-- Showing path of Geom → Prim → Mark → 𝕋 Mark
def GeomUnitCircle : Geom := .circle 1 ![0,0]
def PrimUnitCircle : Prim := {g := GeomUnitCircle , s := "FillBlack"}
def primToArrayPrim ( p : Prim) : Array Prim := #[p]

instance : MarkInterface Prim where
 θ p := primToArrayPrim p

def MarkCircle : Mark := .mk PrimUnitCircle
def 𝕋MarkCircle : 𝕋 Mark := 𝕋.pure MarkCircle

-- Define a type to represent a draw of Plants
structure Plant where
  heigth : Float
  leafs : Mark

instance : MarkInterface Plant where
  θ m := m.leafs.θ

def SimpleTree : Plant := {heigth := 1 , leafs := MarkCircle }
def MarkTree : Mark := .mk SimpleTree

#check MarkTree + MarkTree
--#check MarkTree + MarkCircle
