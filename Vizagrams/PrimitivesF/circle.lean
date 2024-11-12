import Vizagrams.PrimitivesF.graphical_primitives
namespace Circle
open GraphicalPrimitives

-- Type Circle
structure Circle (ℝ : Type) where
  radius : ℝ
  center : (ℝ × ℝ)
deriving Repr

-- Insatanciando transformações lineares para Circle
instance : LinearTransform ( Circle Float ) where
  translation (c : Circle Float ) (v : Float × Float ): Circle Float :=
  let new_center := (c.center.1 + v.1 , c.center.2 + v.2)
  {c with center := new_center}
  rotation (c : Circle Float ) ( θ : Float) := c -- A trabalhar

--struct CovCircle / Circle representado por um diâmetro
structure CircleByDiameter (ℝ : Type) where
  p₁ : (ℝ × ℝ)
  p₂ : (ℝ × ℝ)
deriving Repr

-- Instanciândo transformações lineares para CircleByDiameter
instance : LinearTransform ( CircleByDiameter Float ) where
  translation (c : CircleByDiameter Float ) ( v : Float × Float) :=
  let new_p₁ := ( c.p₁.1 + v.1 , c.p₁.2 + v.2 )
  let new_p₂ := ( c.p₂.1 + v.1 , c.p₂.2 + v.2 )
  {c with p₁ := new_p₁ , p₂ := new_p₂}
  rotation (c : CircleByDiameter Float ) ( θ : Float) := c -- A trabalhar

def getRadius (c : CircleByDiameter Float) : Float :=
  let diameter := norma ( c.p₂.1 - c.p₁.1 , c.p₂.2 - c.p₁.2 )
  diameter/2
