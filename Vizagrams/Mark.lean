import Vizagrams.Primitives
import Vizagrams.FreeMonad

open Primitives
open FreeMonad
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

def Mark.nil :𝕋 Mark  := 𝕋.pure ⟨Circle.o⟩

structure Head where
  size : Float
  smile : Float
deriving Repr
instance : ToString Head where
  toString h := "Head (size: " ++ toString h.size ++ ", smile: " ++ toString h.smile ++ ")"
def Head.o : Head := Head.mk 1.0 0.0

instance : MarkInterface Head where
  θ h :=
    let eyes := (Circle.mk 0.3 ⊞[-0.8,1]) ⊕ (Circle.mk 0.3 ⊞[0.8,1])
    let smile := Line.mk (⊞[-1,-0.5], ⊞[1,-0.5])
    let head := Circle.mk (2*h.size) ⊞[0,0]
    head ⊕ eyes ⊕ smile

def algθ : 𝕋 (Array Prim) → Array Prim
  | 𝕋.pure x => x
  | 𝕋.comp x y => (algθ x) ⊕ (algθ y)
  | 𝕋.act h x => algθ x

def Mark.flat (t : 𝕋 Mark) : Array Prim := algθ (Mark.θ <$> t)

open ProofWidgets Svg in
private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400

open ProofWidgets Svg in
def Mark.draw (t : 𝕋 Mark) (fr : Frame := frame) : ProofWidgets.Html := drawsvg (Mark.flat t) fr

private def x : 𝕋 Mark := pure ⟨Head.o⟩
#eval algθ (Mark.θ <$> x)

private def y :𝕋 Mark  := 𝕋.comp (𝕋.pure ⟨Head.o⟩) (𝕋.comp (𝕋.pure ⟨Circle.o⟩) (𝕋.pure ⟨Line.o⟩))
#eval algθ (Mark.θ <$> y)

private def z :𝕋 Mark  := 𝕋.comp (𝕋.pure ⟨Circle.o⟩) (𝕋.comp (𝕋.pure ⟨Circle.o⟩) (𝕋.pure ⟨Line.o⟩))
#eval algθ (Mark.θ <$> z)

-- #eval Mark.mk Circle.o
-- #eval prim Circle.o
-- #eval prim Circle.o

open ProofWidgets Svg in
private def frame2 : Frame where
  xmin   := -5
  ymin   := -5
  xSize  := 10
  width  := 400
  height := 400

#html drawsvg (algθ (Mark.θ <$> x))
#html Mark.draw y
#html Mark.draw z

structure Adam where
  head : Head
  height : Float
instance : ToString Adam where
  toString h := "Adam (head: " ++ toString h.head ++ ", height: " ++ toString h.height ++ ")"
def Adam.o (head : Head := Head.o) : Adam := Adam.mk head (7 * head.size)

def Adam.ζ (adam : Adam) : 𝕋 Mark :=
  let head := adam.head
  let body := Line.mk (⊞[0.,0.],⊞[0,-adam.height])
  let diag : 𝕋 Mark := 𝕋.comp (𝕋.pure ⟨body⟩) (𝕋.pure ⟨head⟩)
  diag
instance : MarkInterface Adam where
  θ adam := algθ (Mark.θ <$> Adam.ζ adam)

#html Mark.draw (.pure ⟨Adam.o⟩ : 𝕋 Mark) frame2

def w :𝕋 Mark  := 𝕋.comp (𝕋.pure ⟨Adam.o⟩) (𝕋.comp (𝕋.pure ⟨Circle.mk 2.0 ⊞[2,1]⟩) (𝕋.pure ⟨Adam.o⟩))

#html Mark.draw w frame2

instance : HAdd (𝕋 Mark) (𝕋 Mark) (𝕋 Mark) where
  hAdd m1 m2 := 𝕋.comp m1 m2
instance  {α β : Type} [MarkInterface α] [MarkInterface β] [PrimInterface α] [PrimInterface β] [ToString α] [ToString β] : HAdd α β (𝕋 Mark) where
  hAdd p1 p2 := 𝕋.comp (𝕋.pure ⟨p1⟩) (𝕋.pure ⟨p2⟩)
instance  {α : Type} [MarkInterface α] [PrimInterface α] [ToString α] : HAdd α (𝕋 Mark) (𝕋 Mark) where
  hAdd p m := 𝕋.comp (𝕋.pure ⟨p⟩) m
instance  {β : Type} [MarkInterface β] [PrimInterface β] [ToString β] : HAdd (𝕋 Mark) β (𝕋 Mark) where
  hAdd m p := 𝕋.comp m (𝕋.pure ⟨p⟩)

#check x + z
#check Circle.o + x
#html Mark.draw (x+z) frame2
#html Mark.draw ((Circle.mk 2.0 ⊞[3,0]) + x) frame2
#html Mark.draw ((x + Circle.mk 2.0 ⊞[3,0]) + Line.o) frame2
#html Mark.draw ((x + Circle.mk 2.0 ⊞[3,0]) + Line.o)

-- instance  {α : Type} [PrimInterface α] [ToString α] : HAdd  α (Array Prim) (Array Prim) where
--   hAdd p a := #[prim p] ++ a
-- instance  {α : Type} [PrimInterface α] [ToString α] : HAdd  (Array Prim) α (Array Prim) where
--   hAdd a p := a ++ #[prim p]
-- instance  : HAdd  Prim Prim (Array Prim) where
--   hAdd p1 p2 := #[p1, p2]
-- instance  : HAdd  Prim (Array Prim) (Array Prim) where
--   hAdd p1 p2 := #[p1] ++ p2
-- instance  : HAdd  (Array Prim) Prim (Array Prim) where
--   hAdd p1 p2 := p1 ++ #[p2]
-- instance  : HAdd  (Array Prim) (Array Prim) (Array Prim) where
--   hAdd p1 p2 := p1 ++ p2
