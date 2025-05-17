import Vizagrams.Mark
--set_option autoImplicit true
open GraphicalPrimitive
open GraphicalMark
open Sty

namespace FreeMonad

-- Transformar em Tupla
structure ℍ where -- Tranformações Gráficas
  s : Style
  g : Mat2Vec2

instance : Mul ℍ where
  mul x y := ℍ.mk (Style.comp x.s y.s) ( x.g )

inductive F (α : Type) where
  | comp : α → α → F α
  | act : H → α → F α


instance : Functor F where
  map f a := match a with
    | F.comp x y => F.comp (f x) (f y)
    | F.act h x => F.act h (f x)



inductive 𝕋 (α : Type u) where
  | pure : α → 𝕋 α
  | comp : 𝕋 α → 𝕋 α → 𝕋 α
  | act : ℍ → 𝕋 α → 𝕋 α
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

def applyH (h : ℍ) (prims : Array Prim) : Array Prim :=
  prims.map (fun p => h.s * (h.g * p))

def algθ : 𝕋 (Array Prim) → Array Prim
  | 𝕋.pure x => x
  | 𝕋.comp x y => (algθ x) ⊕ (algθ y)
  | 𝕋.act h x => applyH h (algθ x)

def flat (t : 𝕋 Mark) : Array Prim := algθ ((𝕋.map Mark.θ) t)

instance : HMul ℍ (𝕋 Mark) (𝕋 Mark) :=
  ⟨fun h t => 𝕋.act h t⟩

instance : HMul (𝕋 Mark) ℍ (𝕋 Mark) :=
  ⟨fun t h => 𝕋.act h t⟩

instance : Coe Mark (𝕋 Mark) where
  coe m := 𝕋.pure m

instance : HAdd (𝕋 Mark) (𝕋 Mark) (𝕋 Mark) where
  hAdd m1 m2 := 𝕋.comp m1 m2

instance : HAdd Mark (𝕋 Mark) (𝕋 Mark) where
  hAdd m t := 𝕋.comp (𝕋.pure m) t

instance : HAdd (𝕋 Mark) Mark (𝕋 Mark) where
  hAdd t m := 𝕋.comp t (𝕋.pure m)

end FreeMonad
