import SciLean

open SciLean Scalar RealScalar

set_option autoImplicit true
set_default_scalar Float

namespace GeometricTransformation

-- Auxiliar function for Vector Norm
def vecnorm (v : Float^[n]) : Float :=
  (v.foldl (λ acc x => acc + x*x) 0.0).sqrt

instance : HMul Float (Float^[2]) (Float^[2]) where
  hMul x y := ⊞[x * y[0], x * y[1]]
instance : HMul (Float^[2]) Float (Float^[2]) where
  hMul y x := ⊞[x * y[0], x * y[1]]


structure G where
  A : Float^[2,2]
  b : Float^[2]

def G.eval (f : G) (x : Float^[2]) := f.A * x + f.b
def G.comp (f g : G) : G :=
  { A := f.A * g.A, b := f.A * g.b + f.b }

instance : HMul (G) (Float^[2]) (Float^[2]) where
  hMul := G.eval

instance : HMul (G) (G) (G) where
  hMul := G.comp

infixr:80 " ∘ " => G.comp

def G.translate (t : Float^[2]) : G :=
{
  A := 𝐈 2
  b := t
}

def G.rotate (θ : Float) : G :=
{
  A := ⊞[cos θ, -sin θ;sin θ, cos θ]
  b := 0
}

def G.scale (s : Float) : G :=
{
  A := s • 𝐈 2
  b := 0
}

private def examplePoint : Float^[2] := ⊞[1.0, 1.0]  -- Point [1, 1]
#eval examplePoint

def T : G :=
  G.translate ⊞[2.0, 0.0]

def R : G :=
  G.rotate π

def S : G :=
  G.scale 2.0


private def x : Float^[2] := ⊞[0.0, 0.0]  -- Point [1, 1]
#eval x
#eval G.translate ⊞[1.0,0.0] * x
#eval (G.rotate π) * x

#eval T.eval examplePoint
#eval (T.comp T).eval examplePoint
#eval R.eval examplePoint
#eval T * examplePoint
#eval (T ∘ T) * examplePoint
#eval (T ∘ T ∘ T) * examplePoint
#eval T ∘ T ∘ T * examplePoint

#eval (T ∘ T) * x


#eval vecnorm examplePoint
#eval examplePoint[2]

#check G.translate ⊞[1,0]

end GeometricTransformation
