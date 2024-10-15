/-
Monoide:
Um conjunto Ω munido de uma operação ◆ e um elemento ε que satisfazem:
. x ◆ ε = ε ◆ x = x
. x ◆ y ◆ z = (x ◆ y) ◆ z
-/
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

inductive Prim
| circle : Nat → Prim

structure Diagram :=
  (prims : List Prim)

def prim_to_svg : Prim → String
| Prim.circle r => "<circle cx= \"50\" cy=\"50\" r=\"" ++ toString r ++ "\" fill=\"blue\" />"

def diagram_to_svg (d : Diagram) : String :=
  let header := "<svg width=\"100\" height=\"100\" xmlns=\"http://www.w3.org/2000/svg\">"
  let footer := "</svg>"
  let body := String.join (d.prims.map prim_to_svg)
  header ++ body ++ footer

def example_diagram : Diagram :=
  { prims := [Prim.circle 30] }

#eval diagram_to_svg example_diagram
