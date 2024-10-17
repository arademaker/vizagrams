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


structure Circle where
  radius : Nat -- Raio
  center : Nat × Nat -- Centro (x, y)

structure Square where
  side : Nat -- Lado
  center : Nat × Nat -- Ponto Inicial (x, y)

structure Line where
  startline : Nat × Nat -- Ponto inicial (x1, y1)
  endline : Nat × Nat -- Ponto final (x2, y2)
  stroke : String -- Basicamente a cor
  strokeWidth : Nat -- Quão forte é a linha

inductive Prim
  | circle : Circle → Prim
  | square : Square → Prim
  | line : Line → Prim

def Diagram := List Prim

-- Função para gerar SVG de primitivas
def drawPrim : Prim → String
  | Prim.circle c => s!"<circle cx='{c.center.1}' cy='{c.center.2}' r='{c.radius}' />"
  | Prim.square s => s!"<rect x='{s.center.1 - s.side / 2}' y='{s.center.2 - s.side / 2}' width='{s.side}' height='{s.side}' />"
  | Prim.line l => s!"<line x1='{l.startline.1}' y1='{l.startline.2}' x2='{l.endline.1}' y2='{l.endline.2}' stroke='{l.stroke}' stroke-width='{l.strokeWidth}' />"

-- Função que desenha a lista de primitivas em ordem inversa
def draw (diagram : Diagram) : String :=
  let primitives := diagram.reverse.map drawPrim
  let svgContent := String.intercalate "\n" primitives
  s!"<svg xmlns='http://www.w3.org/2000/svg' version='1.1'>\n{svgContent}\n</svg>"

def myDiagram : Diagram := [
  Prim.circle ⟨50, (100, 100)⟩,  -- Círculo com raio 50 e centro em (100, 100)
  Prim.square ⟨40, (120, 120)⟩,  -- Quadrado com lado 40 e centro em (120, 120)
  Prim.line ⟨(50, 50), (150, 150) , "orange" , 5 ⟩  -- Linha de (50, 50) até (150, 150)
]

#eval draw myDiagram
