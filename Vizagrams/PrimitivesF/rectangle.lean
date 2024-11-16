import Vizagrams.PrimitivesF.graphical_primitives
namespace Circle
open GraphicalPrimitives

structure Rectangle (ℝ : Type) where
 origin : (ℝ × ℝ)
 width : ℝ
 height : ℝ
deriving Repr

-- Retangulo pelos pontos
structure CovRectangle (ℝ : Type) where
  v₁ : (ℝ × ℝ ) -- origem
  v₂ : (ℝ × ℝ ) -- width
  v₃ : (ℝ × ℝ ) -- height
deriving Repr

instance : GeomPrim (Rectangle Float) (CovRectangle Float) where
  ψ ( 𝕊 : CovRectangle Float) : (Rectangle Float) :=
  let origin₀ := 𝕊.v₁
  let width₀ := norma ( (𝕊.v₁.1 - 𝕊.v₂.1  , 𝕊.v₁.2 - 𝕊.v₂.2 ))
  let height₀ := norma ( (𝕊.v₁.1 - 𝕊.v₃.1  , 𝕊.v₁.2 - 𝕊.v₃.2 ))
  let Rectangle₀ : Rectangle Float := { origin := origin₀ , width := width₀ , height := height₀}
  Rectangle₀

  φ ( 𝕊 : Rectangle Float) : (CovRectangle Float) :=
  let v1 := 𝕊.origin
  let v2 := (v1.1 + 𝕊.width, v1.2 + 𝕊.width)
  let v3 := (v1.1 + 𝕊.height, v1.2 + 𝕊.height)
  let CovRectangle₁ : CovRectangle Float := { v₁ := v1 , v₂ := v2 , v₃ := v3 }
  CovRectangle₁
