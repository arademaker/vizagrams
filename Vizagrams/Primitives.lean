

namespace Primitive

structure circle (α : Type) where
 center : (α × α)
 radious : α
deriving Repr

-- Função para calcular o comprimento de um vetor
def magnitude (v : Float × Float) : Float :=
  Float.sqrt (v.1 * v.1 + v.2 * v.2) -- ⟨ v , v ⟩  ou vᵀv ou ‖v‖

def vector_v := ( (3 : Float ), (4 : Float))
#eval magnitude ( vector_v )

-- Função para normalizar um vetor
def normalize (v : Float × Float) : Float × Float :=
  let mag := magnitude v
  (v.1 / mag, v.2 / mag) -- v/‖v‖

#eval normalize vector_v
def v_unit := normalize vector_v
#eval magnitude v_unit

-- Função envelope para o círculo
def envelope_circle (c : circle Float) (v : Float × Float) : Float :=
  let v_norm := normalize v
  let p := (c.center.1 + c.radious * v_norm.1, c.center.2 + c.radious * v_norm.2)
  p.1 * v_norm.1 + p.2 * v_norm.2  -- Produto escalar para projeção

def Circle_1 : circle Float := { center := (4, 3), radious := 5.0 }
#eval envelope_circle Circle_1 (12,15)

structure rectangle (α : Type) where
 origin : (α × α)
 width : α
 height : α
deriving Repr

-- Função envelope para o retângulo
def envelope_rectangle (r : rectangle Float) (v : Float × Float) : Float :=
  let v_norm := normalize v
  let corners := [
    (r.origin.1, r.origin.2),  -- canto inferior esquerdo
    (r.origin.1 + r.width, r.origin.2),  -- canto inferior direito
    (r.origin.1, r.origin.2 + r.height),  -- canto superior esquerdo
    (r.origin.1 + r.width, r.origin.2 + r.height)  -- canto superior direito
  ]
  corners.map (λ p => p.1 * v_norm.1 + p.2 * v_norm.2) |>.foldl max corners.head!.1  -- Projeta cada canto e retorna o máximo

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

structure QBezier ( α : Type ) where
  basePoints : List (α × α)
  controlPoints : List (α × α)
  -- Verificar se len(basepoints < controlPoints) e se len(basePoints ≥ 2)
deriving Repr

structure Style where
  stroke : String
  strokeWidth : Nat
  fill : String
  opacity : Float
  deriving Repr

/-
structure path (α : Type) where
  d : String  -- Definição do caminho como string de comandos SVG
deriving Repr
-/

end Primitive
