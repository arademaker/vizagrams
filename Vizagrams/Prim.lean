import Vizagrams.Geom
import Vizagrams.Style

namespace GraphicalPrimitive

structure Prim where
  geom : GeometricPrimitive.Geom
  style : Sty.Style
deriving Repr

instance : Coe Prim (Array Prim) where
  coe p := #[p]

instance : Coe Prim GeometricPrimitive.Geom where
 coe p := p.geom

/- Estrutura que representa uma transformaçãoa a uma primitiva Gráfica,
isto é, uma transformação geométrica e uma transformação de estilo -/
structure H where
  f : Mat2Vec2
  s : Sty.Style
deriving Repr

instance : HMul Mat2Vec2 Prim Prim where
  hMul g p := {p with geom := g * p.geom}

instance : HMul Mat2Vec2 (Array Prim) (Array Prim) where
  hMul g ps := ps.map (λ p => g * p)

instance : HMul Sty.Style Prim Prim where
  hMul s p := {p with style := Sty.Style.comp s  p.style}

def applyS (s : Sty.Style) (p : Prim) : Prim :=
  s * p

def applySArray (s : Sty.Style) (ps : Array Prim) : Array Prim :=
  ps.map (fun p => s * p)

def applyG (g : Mat2Vec2) (p : Prim) : Prim :=
  g * p

def applyGArray (g : Mat2Vec2) (ps : Array Prim) : Array Prim :=
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

end GraphicalPrimitive
