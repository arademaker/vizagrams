import Vizagrams.PrimitivesF.graphical_primitives
namespace Circle
open GraphicalPrimitives

-- Type Circle
structure Circle (ℝ : Type) where
  radius : ℝ
  center : (ℝ × ℝ)
deriving Repr

--struct CovCircle / Circle representado por um diâmetro
structure CovCircle (ℝ : Type) where
  p₁ : (ℝ × ℝ)
  p₂ : (ℝ × ℝ)
deriving Repr

-- Funções auxiliares
def getRadius (c : CovCircle Float) : Float :=
  let diameter := norma ( c.p₂.1 - c.p₁.1 , c.p₂.2 - c.p₁.2 )
  diameter/2

def getCenter (c : CovCircle Float) : Float × Float:=
  let center := ( (c.p₂.1 + c.p₁.1)/2 , (c.p₂.2 + c.p₁.2)/2 )
  center
--

-- Instanciar
instance : GeomPrim (Circle Float) (CovCircle Float) where
  ψ ( ℂ : CovCircle Float) : (Circle Float) :=
  let radius₀ := getRadius ( ℂ )
  let center₀ := getCenter ( ℂ )
  let circle₀ : Circle Float := { radius := radius₀ , center := center₀ }
  circle₀

  φ ( ℂ : Circle Float) : (CovCircle Float) :=
  let pt₁ := ( ℂ.center.1 + ℂ.radius , ℂ.center.2 )
  let pt₂ := ( ℂ.center.1 - ℂ.radius , ℂ.center.2 )
  let cov : CovCircle Float := { p₁ := pt₁ , p₂ := pt₂ }
  cov

-- Insatanciando transformações lineares para Circle
instance : AfineTransforms ( Circle Float ) where
  translation (c : Circle Float ) (v : Float × Float ): Circle Float :=
  let new_center := (c.center.1 + v.1 , c.center.2 + v.2)
  {c with center := new_center}
  rotation (c : Circle Float ) ( θ : Float) := c -- A trabalhar


-- Instanciândo transformações lineares para CircleByDiameter
instance : AfineTransforms ( CovCircle Float ) where
  translation (c : CovCircle Float ) ( v : Float × Float) :=
  let new_p₁ := ( c.p₁.1 + v.1 , c.p₁.2 + v.2 )
  let new_p₂ := ( c.p₂.1 + v.1 , c.p₂.2 + v.2 )
  {c with p₁ := new_p₁ , p₂ := new_p₂}
  rotation (c : CovCircle Float ) ( θ : Float) := c -- A trabalhar
