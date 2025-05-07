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

/- # Mesma discursão a respeito de FreeMonad 𝕋
Em `transformation.lean` é definido o seguinte type

structure G where
  A : Float^[2,2]
  b : Float^[2]

E então são criados os

def G.eval (f : G) (x : Float^[2]) := f.A * x + f.b
def G.comp (f g : G) : G :=
  { A := f.A * g.A, b := f.A * g.b + f.b }

Entretanto, aqui optei por definir uma class `AffineMapClass` pois assim é possível ver
uma maior relação com Category Theory (CT).
𝓒 = ⟨ Vec2 , G em AffineMapClass ⟩
-/
class AffineMapClass (G : Type) where
  eval : G → Vec2 → Vec2
  compose : G → G → G

-- Aqui definimos um AffineMat, isto é, um tipo para representar x ↦ Ax + B
structure AffineMat where
  A : Mat2
  b : Vec2
deriving Repr
-- # Produto de Matrizes e Vetores em Float²
def mulVec (A : Mat2) (v : Vec2) : Vec2 :=
  fun i => (A i 0) * v 0 + (A i 1) * v 1

def matMul (A B : Mat2) : Mat2 :=
  fun i j => A i 0 * B 0 j + A i 1 * B 1 j

def nullVec2 : Vec2 := ![0,0]

#eval mulVec M_Float v₁
#eval matMul M_Float M_Float

/- # Instanciando AffineMapClass para AffineMat
Seja f : V² → V² uma trasformação linear tal que f(x) := Ax + b
portanto instanciamos o campo `eval f x` como `Ax + b`
Agora seja g : V² → V² tal que g(x) := Cx + d, temos que
(f ∘ g)(x) = f(g(x)) = f( Cx + d ) = A ( Cx + d ) + b = (A·C)x + (A · d + b)
-/
instance : AffineMapClass AffineMat where
  eval f x := mulVec f.A x + f.b
  compose f g := { A := matMul f.A g.A , b := (mulVec f.A g.b) + f.b }

infixr:70 " ⬝ " => AffineMapClass.eval
infixr:70 " ∘ " => AffineMapClass.compose

def I2 : Mat2 := !![1, 0; 0, 1]

def translate (t : Vec2) : AffineMat :=
  { A := I2, b := t }

def scale (s : Float) : AffineMat :=
  { A := !![s, 0; 0, s], b := ![0.0, 0.0] }

def rotate (θ : Float) : AffineMat :=
  let c := Float.cos θ
  let s := Float.sin θ
  { A := !![c, -s; s, c], b := ![0.0, 0.0] }

def Float.pi : Float := 3.141592653589793

private def T := translate ![2.0, 0.0]
private def R := rotate (Float.pi / 2)
private def S := scale 2.0
private def v : Vec2 := ![1.0, 1.0]

#eval (T ∘ R ∘ S) ⬝ v
