import Vizagrams.LeannearAlgebra

private def v₁ : Vec2 := ![1, 1]
#eval v₁
#eval s!"M {getCoordinates v₁}"

private def M_Float : Matrix (Fin 2) (Fin 2) Float := !![1 ,1 ; 1,0]
-- Usar `!![ ; ]` vem de Matrix.Notation
#eval M_Float

#eval Vec2Norm v₁

#eval normalize v₁

#eval mulVec M_Float v₁
#eval matMul M_Float M_Float

private def T := translate ![2.0, 0.0]
private def R := rotate (π / 2)
private def S := scale 2.0
private def v : Vec2 := ![1.0, 1.0]

#eval (T ∘ R ∘ S) ⬝ v

def 𝕍 : Vec2 × Vec2 := {fst := v₁ , snd := 2 * v₁}
#eval getCoordinates 𝕍.fst
