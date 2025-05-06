import Mathlib.Data.Matrix.Basic
import Mathlib.Data.Matrix.Notation

open Matrix Fin
abbrev Vec2 := Fin 2 → Float
/-# Fin
Fin n representa o conjunto `Iₙ` ou `[n]`, isto é, os Naturais menores que n
podemos usar `Fin n` para indexar vetores com n posições
`Fin 2 → ℝ²` é o vetor de duas entradas reais
-/
private def v₁ : Vec2 := ![1, 1]
#eval v₁

abbrev Mat2 := Matrix (Fin 2) (Fin 2) Float
/-# Mat2
`Defs.lean:` (Mathlib.Data.Matrix)
def Matrix (m : Type u) (n : Type u') (α : Type v) : Type max u u' v :=
  m → n → α

uma matriz é uma função que, dado um índice de linha `m` e um de coluna `n`,
retorna o elemento daquela posição
-/
private def M_Float : Matrix (Fin 2) (Fin 2) Float := !![1 ,1 ; 1,0]
-- Usar `!![ ; ]` vem de Matrix.Notation
#eval M_Float

-- # Definir Norma de vetor
def vec2Norm (v : Vec2) : Float :=
  Float.sqrt (v 0 ^ 2 + v 1 ^ 2)

#eval vec2Norm v₁

-- # Normalização de vetor
def normalize (v : Vec2) : Vec2 :=
  let n := vec2Norm v
  if n == 0 then v else fun i => v i / n -- `==` igualdade booleana

#eval normalize v₁




def A : Matrix (Fin 2) (Fin 2) ℕ :=
  ![![1, 2],
    ![3, 4]]

#eval A 0 1      -- resultado: 2
#eval (Aᵀ) 1 0   -- resultado: 2 (Aᵀ é transposta)
