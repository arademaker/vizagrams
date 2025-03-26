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

instance : Coe Prim (Array Prim) where
  coe p := #[p]

/- Estrutura que representa uma transformaçãoa uma primitiva Gráfica,
isto é, uma transformação geométrica e uma transformação de estilo -/
structure H where
  g : G
  s : Style

instance : Repr H where
    reprPrec h _ := "{ g:= G, s := " ++ repr h.s ++ " }"



instance : HMul G Prim Prim where
  hMul g p := {p with geom := g * p.geom}

instance : HMul Style Prim Prim where
  hMul s p := {p with s := Style.comp s  p.s}

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
