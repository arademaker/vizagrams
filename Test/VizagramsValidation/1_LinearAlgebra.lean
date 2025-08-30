/- # header
-/
import Vizagrams.LeannearAlgebra
/-! # Doc
-/

-- # Defining a 2-D Vector
open LinearAlgebra

def v₁ : Vec2 := ![5.0 ,3.0]
#check v₁
#eval v₁
#eval ⟪ v₁, v₁ ⟫
#eval ‖ v₁ ‖
#eval ‖ v₁ ‖²

def v₂ : Vec2 := ![2.0, 6.0]
#eval ⟪ v₁ , v₂ ⟫
#eval ⟪ v₂ , v₁ ⟫

#eval e₁ + e₂
#eval normalize v₁
#eval ‖ (normalize v₁) ‖
#eval normalize nullVec2
#eval normalize (1/2 * e₁)

def A₁ : Mat2 := !![2.0 ,6.0 ;7.0 ,3.0]
#check A₁
#eval A₁
