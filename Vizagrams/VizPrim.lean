import Vizagrams.Style
import Vizagrams.VizGeom

namespace GraphicalPrimitive
open Sty GeometricPrimitive GeometricTransformation

/-
Uma primitiva Gráfica é a junção de uma primitiva geométrica e sua primitiva de estilo -/
structure Prim where
  geom : Geom
  s := ({} : Style) -- Uses the default Style none none none
deriving Repr

instance : ToString Prim where
  toString p := s!"Prim"

instance : Coe Prim (Array Prim) where
  coe p := #[p]

instance : Coe Prim GeometricPrimitive.Geom where
 coe p := p.geom

/- Estrutura que representa uma transformaçãoa a uma primitiva Gráfica,
isto é, uma transformação geométrica e uma transformação de estilo -/
structure H where
  g : G
  s : Style

instance : Repr H where
    reprPrec h _ := "{ g:= G, s := " ++ repr h.s ++ " }"

instance : HMul G Prim Prim where
  hMul g p := {p with geom := g * p.geom}

instance : HMul G (Array Prim) (Array Prim) where
  hMul g ps := ps.map (λ p => g * p)

instance : HMul Style Prim Prim where
  hMul s p := {p with s := Style.comp s  p.s}

def applyS (s : Style) (p : Prim) : Prim :=
  s * p

def applySArray (s : Style) (ps : Array Prim) : Array Prim :=
  ps.map (fun p => s * p)

def applyG (g : G) (p : Prim) : Prim :=
  g * p

def applyGArray (g : G) (ps : Array Prim) : Array Prim :=
  ps.map (fun p => g * p)


class HPlus (α : Type u) (β : Type v) where
  hPlus : α → β → Array Prim

instance : HPlus  Prim Prim where
  hPlus p1 p2 := #[p1, p2]
instance  : HPlus  Prim (Array Prim) where
  hPlus p1 p2 := #[p1] ++ p2
instance  : HPlus  (Array Prim) Prim where
  hPlus p1 p2 := p1 ++ #[p2]
instance  : HPlus  (Array Prim) (Array Prim) where
  hPlus p1 p2 := p1 ++ p2

infixr:80 " ⊕ " => HPlus.hPlus

def boundingBoxPrim (p : Prim) : BoundingBox :=
  boundingBox p.geom

/-- União de bounding‐boxes de um array de Prim. -/
def boundingBoxPrims (ps : Array Prim) : BoundingBox :=
  if h : ps.size > 0 then
    let firstBB := boundingBoxPrim ps[0]
    ps.foldl (fun acc p => acc.union (boundingBoxPrim p)) firstBB
  else
    -- Array vazio: BB degenerado na origem
    { lower := ⊞[0.0,0.0], upper := ⊞[0.0,0.0] }

end GraphicalPrimitive
