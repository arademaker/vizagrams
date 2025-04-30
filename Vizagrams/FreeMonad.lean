import Vizagrams.VizPrim
import Vizagrams.VizMark
import Vizagrams.Transformations
import Vizagrams.Style
--set_option autoImplicit true
open GraphicalPrimitive
open GraphicalMark
open Sty
open GeometricTransformation
namespace FreeMonad

-- Transformar em Tupla
structure H where -- Tranformações Gráficas
  s : Style
  g : G

instance : Mul H where
  mul x y := H.mk (Style.comp x.s y.s) ( x.g )

inductive F (α : Type) where
  | comp : α → α → F α
  | act : H → α → F α
--deriving Repr, BEq

instance : Functor F where
  map f a := match a with
    | F.comp x y => F.comp (f x) (f y)
    | F.act h x => F.act h (f x)

/-
instance : Mul H where
  mul x y := H.mk (x.g * y.g)
-/

inductive 𝕋 (α : Type u) where
  | pure : α → 𝕋 α
  | comp : 𝕋 α → 𝕋 α → 𝕋 α
  | act : H → 𝕋 α → 𝕋 α
-- deriving Repr, BEq

def 𝕋.map (f : α → β) (a : 𝕋 α) : 𝕋 β :=
  match a with
  | 𝕋.pure x => 𝕋.pure (f x)
  | 𝕋.comp x y => 𝕋.comp (𝕋.map f x) (𝕋.map f y)
  | 𝕋.act h x => 𝕋.act h (𝕋.map f x)

instance : Functor 𝕋 where
  map := 𝕋.map

def η : α → 𝕋 α := fun a => 𝕋.pure a

def μ : 𝕋 (𝕋 α) → 𝕋 α
  | 𝕋.pure x => x
  | 𝕋.comp x y => 𝕋.comp (μ x) (μ y)
  | 𝕋.act h x =>
    match x with
      | 𝕋.act h' y => 𝕋.act (h * h') (μ y)
      | a => μ a

def freebind : (𝕋 α) → (α → 𝕋 β) → (𝕋 β) :=
  fun ma f => (μ ∘ (𝕋.map f)) ma

instance : Monad 𝕋 where
  pure := η
  bind := freebind

/-
def algF : F Float → Float
 | F.comp x y => x + y
 | F.act h y => h.g * y

def alg : 𝕋 Float → Float
  | 𝕋.pure x => x
  | 𝕋.comp x y => (alg x) + (alg y)
  | 𝕋.act h x => h.g * (alg x)
-/

def applyH (h : H) (prims : Array Prim) : Array Prim :=
  prims.map (fun p => h.s * (h.g * p))

def algθ : 𝕋 (Array Prim) → Array Prim
  | 𝕋.pure x => x
  | 𝕋.comp x y => (algθ x) ⊕ (algθ y)
  | 𝕋.act h x => applyH h (algθ x)

def flat (t : 𝕋 Mark) : Array Prim := algθ ((𝕋.map Mark.θ) t)

instance : HMul H (𝕋 Mark) (𝕋 Mark) :=
  ⟨fun h t => 𝕋.act h t⟩

instance : HMul (𝕋 Mark) H (𝕋 Mark) :=
  ⟨fun t h => 𝕋.act h t⟩

instance : Coe Mark (𝕋 Mark) where
  coe m := 𝕋.pure m

instance : HAdd (𝕋 Mark) (𝕋 Mark) (𝕋 Mark) where
  hAdd m1 m2 := 𝕋.comp m1 m2

instance : HAdd Mark (𝕋 Mark) (𝕋 Mark) where
  hAdd m t := 𝕋.comp (𝕋.pure m) t

instance : HAdd (𝕋 Mark) Mark (𝕋 Mark) where
  hAdd t m := 𝕋.comp t (𝕋.pure m)

def boundingBox𝕋 (t : 𝕋 Mark) : GeometricPrimitive.BoundingBox :=
  boundingBoxPrims (flat t)

def envelopePositionMarks (𝕄₁ : 𝕋 Mark) ( v : Float^[2]) (𝕄₂ : 𝕋 Mark) (gap : Float := 0): 𝕋 Mark :=
  let 𝕞₁ := flat 𝕄₁
  let 𝕞₂ := flat 𝕄₂
  let v₁ := normalize v
  let offset := (envelopeArray 𝕞₁ v₁) + (envelopeArray 𝕞₂ (-v₁)) + gap
  let position := offset * v₁
  let h : H := { s := {} , g := G.translate position }
  h * 𝕄₂

/-- Coloca `p₂` à direita do array `A`, alinhando pela direção (1,0). -/
def hStackRightMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ⊞[1,0] 𝕄₂ gap)

/-- Coloca `p₂` à direita do array `A`, alinhando pela direção (1,0). -/
def hStackLeftMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ⊞[-1,0] 𝕄₂ gap)

def vStackUpMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ⊞[0,1] 𝕄₂ gap)

def vStackDownMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ⊞[0,-1] 𝕄₂ gap)

infixr:70 " → " => hStackRightMarks
infixr:70 " ← " => hStackLeftMarks
infixr:70 " ↑ " => vStackUpMarks
infixr:70 " ↓ " => vStackDownMarks

notation:70 A " →[" g "] " B => hStackRightMarks A B g
notation:70 A " ←[" g "] " B => hStackLeftMarks  A B g
notation:70 A " ↑[" g "] " B => vStackUpMarks    A B g
notation:70 A " ↓[" g "] " B => vStackDownMarks  A B g

end FreeMonad
