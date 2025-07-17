import Mathlib.Data.Matrix.auto
import Mathlib.Data.Matrix.Notation

open Matrix Fin
def π : Float := 3.141592653589793 -- Aproximação para π em Float
abbrev Vec2 := Fin 2 → Float
/-# Fin
Fin n representa o conjunto `Iₙ` ou `[n]`, isto é, os Naturais menores que n
podemos usar `Fin n` para indexar vetores com n posições
`Fin 2 → ℝ²` é o vetor de duas entradas reais
-/

abbrev Mat2 := Matrix (Fin 2) (Fin 2) Float
/-# Mat2
`Defs.lean:` (Mathlib.Data.Matrix)
def Matrix (m : Type u) (n : Type u') (α : Type v) : Type max u u' v :=
  m → n → α

uma matriz é uma função que, dado um índice de linha `m` e um de coluna `n`,
retorna o elemento daquela posição
-/
-- Usar `!![ ; ]` vem de Matrix.Notation

-- # Definir Norma de vetor
def Vec2Norm (v : Vec2) : Float :=
  Float.sqrt (v 0 ^ 2 + v 1 ^ 2)

-- # Normalização de vetor
def normalize (v : Vec2) : Vec2 :=
  let n := Vec2Norm v
  if n == 0 then v else fun i => v i / n

class AffineMapLike (G : Type) (V : Type) where
  eval : G → V → V
  compose : G → G → G

-- Aqui definimos um Mat2Vec2, isto é, um tipo para representar x ↦ Ax + B
structure Mat2Vec2 where
  A : Mat2
  b : Vec2
deriving Repr

-- # Produto de Matrizes e Vetores em Float²
def dotProd (v₁ v₂ : Vec2) : Float :=
  v₁ 0 * v₂ 0 + v₁ 1 * v₂ 1

def ScalarMul (s : Float) (v : Vec2) : Vec2 :=
  fun i => s * v i

def mulVec (A : Mat2) (v : Vec2) : Vec2 :=
  fun i => (A i 0) * v 0 + (A i 1) * v 1

def matMul (A B : Mat2) : Mat2 :=
  fun i j => A i 0 * B 0 j + A i 1 * B 1 j

def nullVec2 : Vec2 := ![0,0]

def rotateVec (v : Vec2) (θ : Float) : Vec2 :=
  let cosθ := Float.cos θ
  let sinθ := Float.sin θ
  ![cosθ * v 0 - sinθ * v 1, sinθ * v 0 + cosθ * v 1]

def pointOnEllipse (θ rx ry : Float) : Vec2 :=
  ![rx * Float.cos θ, ry * Float.sin θ]

def atan2pi (v : Vec2) : Float :=
  Float.atan2 (v 1) (v 0)


/- # Instanciando AffineMapClass para Mat2Vec2
Seja f : V² → V² uma trasformação linear tal que f(x) := Ax + b
portanto instanciamos o campo `eval f x` como `Ax + b`
Agora seja g : V² → V² tal que g(x) := Cx + d, temos que
(f ∘ g)(x) = f(g(x)) = f( Cx + d ) = A ( Cx + d ) + b = (A·C)x + (A · d + b)
-/

instance : AffineMapLike Mat2Vec2 Vec2 where
  eval f x := mulVec f.A x + f.b
  compose f g := { A := matMul f.A g.A , b := (mulVec f.A g.b) + f.b }

infixr:70 " ⬝ " => AffineMapLike.eval (G := Mat2Vec2) (V := Vec2)
infixr:70 " ∘ " => AffineMapLike.compose (G := Mat2Vec2) (V := Vec2)


def I2 : Mat2 := !![1, 0; 0, 1]

def translate (t : Vec2) : Mat2Vec2 :=
  { A := I2, b := t }

def scale (s : Float) : Mat2Vec2 :=
  { A := !![s, 0; 0, s], b := ![0.0, 0.0] }

def rotate (θ : Float) : Mat2Vec2 :=
  let c := Float.cos θ
  let s := Float.sin θ
  { A := !![c, -s; s, c], b := ![0.0, 0.0] }

def getCoordinates (v : Vec2) : String :=
  s!"{v 0} {v 1}"
