set_option autoImplicit true

namespace FreeMonad
structure H where
  g : Float
deriving Repr, BEq
instance : Mul H where
  mul x y := H.mk (x.g * y.g)

inductive F (α : Type) where
  | comp : α → α → F α
  | act : H → α → F α
deriving Repr, BEq

instance : Functor F where
  map f a := match a with
    | F.comp x y => F.comp (f x) (f y)
    | F.act h x => F.act h (f x)

instance : Mul H where
  mul x y := H.mk (x.g * y.g)

inductive 𝕋 (α : Type u) where
  | pure : α → 𝕋 α
  | comp : 𝕋 α → 𝕋 α → 𝕋 α
  | act : H → 𝕋 α → 𝕋 α
deriving Repr, BEq

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

private def y := 𝕋.comp (𝕋.pure 1) (𝕋.comp (𝕋.pure 2) (𝕋.pure 10))
#eval y
#eval (fun x : Nat => 2 * x) <$> y
#eval toString <$> y

def algF : F Float → Float
 | F.comp x y => x + y
 | F.act h y => h.g * y

def alg : 𝕋 Float → Float
  | 𝕋.pure x => x
  | 𝕋.comp x y => (alg x) + (alg y)
  | 𝕋.act h x => h.g * (alg x)

private def z := 𝕋.comp (𝕋.pure 1.0) (𝕋.comp (𝕋.pure 2) (𝕋.pure 10))
#eval alg z

private def w := 𝕋.act (H.mk 2.5) (𝕋.comp (𝕋.pure 2) (𝕋.pure 10))
#eval alg (Nat.toFloat <$> w)

end FreeMonad
