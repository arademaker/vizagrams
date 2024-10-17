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

-- draw of a simple circle
inductive Prim
| circle : Nat → Prim

structure Diagram :=
  (prims : List Prim)

-- Função que converte primitivos em SVG, levando em conta padding e cor
def prim_to_svg (pad : Nat) (color : String) : Prim → String
| Prim.circle r =>
    let adjusted_cx := toString (50 + pad)
    let adjusted_cy := toString (50 + pad)
    "<circle cx=\"" ++ adjusted_cx ++ "\" cy=\"" ++ adjusted_cy ++ "\" r=\"" ++ toString r ++ "\" fill=\"" ++ color ++ "\" />"

-- Função que converte um Diagrama em SVG, com parâmetros height, pad e color
def draw (d : Diagram) (height : Nat := 100) (pad : Nat := 0) (color : String := "blue") : String :=
  let header := "<svg width=\"" ++ toString height ++ "\" height=\"" ++ toString height ++ "\" xmlns=\"http://www.w3.org/2000/svg\">"
  let footer := "</svg>"
  let body := String.join (d.prims.map (prim_to_svg pad color))
  header ++ body ++ footer

def Circle (r : Nat := 50) : Diagram :=
  { prims := [Prim.circle r] }

def main : IO Unit :=
  IO.println (draw Circle (height := 100) (pad:=20) (color:="red"))

#eval main
