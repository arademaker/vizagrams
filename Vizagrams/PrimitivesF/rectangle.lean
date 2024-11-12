import Vizagrams.PrimitivesF.graphical_primitives
namespace Circle
open GraphicalPrimitives

structure Rectangle (ℝ : Type) where
 origin : (ℝ × ℝ)
 width : ℝ
 height : ℝ
deriving Repr

instance : LinearTransform (Rectangle Float) where
  translation (r : Rectangle Float) (v : Float × Float) :=
  let new_origin := ( r.origin.1 + v.1 , r.origin.2 + v.2)
  { r with origin := new_origin }
  rotation (r : Rectangle Float) ( θ : Float) := r -- A trabalhar

-- Retangulo pelos pontos
structure RectangleByPoints (ℝ : Type) where
  v₁ : (ℝ × ℝ )
  v₂ : (ℝ × ℝ )
  v₃ : (ℝ × ℝ ) -- Determina a altura do retângulo
