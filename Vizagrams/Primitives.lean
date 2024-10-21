/-
Monoide:
Um conjunto Ω munido de uma operação ◆ e um elemento ε que satisfazem:
. x ◆ ε = ε ◆ x = x
. x ◆ y ◆ z = (x ◆ y) ◆ z

class Monoid (α : Type) :=
  ( op : α → α → α)  -- Operação binária
  ( ε : α)          -- Elemento neutro
  (assoc : ∀ a b c : α, op (op a b) c = op a (op b c))  -- Propriedade associativa
  (id_left : ∀ a : α, op ε a = a)                     -- Elemento neutro à esquerda
  (id_right : ∀ a : α, op a ε  = a)                    -- Elemento neutro à direita

instance : Monoid Nat :=
{
  op := Nat.add,  -- A operação será a adição
  ε  := 0,        -- O elemento neutro será o zero
  assoc := Nat.add_assoc,  -- Usamos a associatividade da adição de naturais
  id_left := Nat.zero_add, -- Propriedade: ε + a = a
  id_right := Nat.add_zero -- Propriedade: a + ε  = a
}
-/
namespace Primitive

structure circle (α : Type) where
 center : (α × α)
 radious : α
deriving Repr

structure rectangle (α : Type) where
 origin : (α × α)
 width : α
 height : α
deriving Repr

structure ellipse (α : Type) where
  center : (α × α)
  rx : α
  ry : α
deriving Repr

structure line (α : Type) where
  starting : (α × α)
  ending : (α × α)
deriving Repr

structure polyline (α : Type) where
  points : List (α × α)
deriving Repr

structure polygon (α : Type) where
  points : List (α × α)
deriving Repr
/-
structure path (α : Type) where
  d : String  -- Definição do caminho como string de comandos SVG
deriving Repr
-/
end Primitive
