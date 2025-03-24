import Vizagrams.Transformations

open GeometricTransformation
namespace GeometricPrimitive


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


end GeometricPrimitive
