import Vizagrams.Transformations

open GeometricTransformation
namespace GeometricPrimitive
set_option diagnostics true

-- Utilizamos uma Inductive pois o conjunto de Primitivas Geométricas é finito
inductive Geom where
  | line     (src trg : Float^[2])
  | circle   (r : Float) (c : Float^[2])
  | polyline (points : Array (Float^[2])) -- (type : PolylineType)
  | polygon  (points : Array (Float^[2]))

instance : Repr Geom where
  reprPrec
    | .line src trg, _ => "Geom.line " ++ toString src ++ " " ++ toString trg
    | .circle r c, _ => "Geom.circle " ++ toString r ++ " " ++ toString c
    | .polyline points, _ => "Geom.polyline " ++ toString points
    | .polygon points, _ => "Geom.polygon " ++ toString points

-- É mais fácil trabalhar tansformações geométricas utilizando suas formas covariantes
inductive CovGeom where
  | line     (src trg : Float^[2])
  | circle   (p1 p2 : Float^[2])
  | polyline (points : Array (Float^[2])) -- (type : PolylineType)
  | polygon  (points : Array (Float^[2]))

instance : Repr CovGeom where
  reprPrec
    | .line src trg, _ => "CovGeom.line " ++ toString src ++ " " ++ toString trg
    | .circle r c, _ => "CovGeom.circle " ++ toString r ++ " " ++ toString c
    | .polyline points, _ => "CovGeom.polyline " ++ toString points
    | .polygon points, _ => "CovGeom.polygon " ++ toString points

def ϕ : Geom → CovGeom
  | .line src trg => .line src trg
  | .circle r c => .circle (c - ⊞[r, 0]) (c + ⊞[r, 0])
  | .polyline points => .polyline points
  | .polygon points => .polygon points

def ψ : CovGeom → Geom
  | .line src trg => .line src trg
  | .circle p1 p2 => .circle (vecnorm (p1 - p2) / 2) ((p1 + p2) * 0.5)
  | .polyline points => .polyline points
  | .polygon points => .polygon points

instance : HMul G CovGeom CovGeom where
  hMul g p := match p with
    | .line src tgt => .line (g*src) (g*tgt)
    | .circle p1 p2 => .circle (g*p1) (g*p2)
    | .polyline points => .polyline (Array.map (g * ·) points)
    | .polygon points => .polygon (Array.map (g * ·) points)

instance : HMul G Geom Geom where
  hMul g p := ψ (g * (ϕ p))

-- Auxiliar: calcula envelope de um array de pontos,
def envelopePts (pts : Array (Float^[2])) (v : Float^[2]) : Float :=
  if h : pts.size > 0 then
    -- índice 0 está garantido existir
    let first := pts[0]!
    let init  := v.dot first
    pts.foldl (fun acc p => max acc (v.dot p )) init
  else
    0.0

-- Envelope geral de qualquer `Geom` na direção `v`.
def envelope (g : Geom) (v' : Float^[2]) : Float :=
  let v := normalize v'
  match g with
  | .circle r c   => v.dot (c + r • v)
  | .line   p q   => max (v.dot p ) (v.dot q )
  | .polyline pts => pts.foldl (fun acc p => max acc (v.dot p ))  (v.dot pts[0]! )
  | .polygon  pts => pts.foldl (fun acc p => max acc (v.dot p ))  (v.dot pts[0]! )


/-- Retângulo envoltório (bounding box) dado pelos limites mínimo e máximo em x e y. -/
structure BoundingBox where
  lower : Float^[2]  -- canto inferior‐esquerdo
  upper : Float^[2]  -- canto superior‐direito

def boundingBox (g : Geom) : BoundingBox :=
  let ex₁ := envelope g (⊞[1.0, 0.0])
  let ex₂ := envelope g (⊞[-1.0, 0.0])
  let ey₁ := envelope g (⊞[0.0, 1.0])
  let ey₂ := envelope g (⊞[0.0, -1.0])
  { lower := ⊞[-ex₂, -ey₂]
  , upper := ⊞[ ex₁,  ey₁] }

def BoundingBox.union (b₁ b₂ : BoundingBox) : BoundingBox :=
  let lx := min (b₁.lower[0]) (b₂.lower[0])
  let ly := min (b₁.lower[1]) (b₂.lower[1])
  let ux := max (b₁.upper[0]) (b₂.upper[0])
  let uy := max (b₁.upper[1]) (b₂.upper[1])
  { lower := ⊞[lx, ly], upper := ⊞[ux, uy] }

def boundingBoxGroup (gs : Array Geom) : BoundingBox :=
  if h : gs.size > 0 then
    let firstBB := boundingBox gs[0]     -- boundingBox de um único Geom
    gs.foldl (fun acc g => acc.union (boundingBox g)) firstBB
  else
    { lower := ⊞[0.0, 0.0], upper := ⊞[0.0, 0.0] }

def envelopePosition (g₁ : Geom) ( v : Float^[2] ) ( g₂ : Geom ) : Geom :=
  let v₁ := normalize v
  let offset := (envelope g₁ v₁) + (envelope g₂ (-v₁))
  let position := offset * v₁
  GeometricTransformation.G.translate position * g₂

/-- Coloca `g₂` à direita de `g₁`, alinhando pela direção (1,0). -/
def hStackRight (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ⊞[1,0] g₂

/-- Coloca `g₂` à esquerda de `g₁`, alinhando pela direção (-1,0). -/
def hStackLeft (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ⊞[-1,0] g₂

/-- Coloca `g₂` acima de `g₁`, alinhando pela direção (0,1). -/
def vStackUp (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ⊞[0,1] g₂

/-- Coloca `g₂` abaixo de `g₁`, alinhando pela direção (0,-1). -/
def vStackDown (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ⊞[0,-1] g₂

end GeometricPrimitive
