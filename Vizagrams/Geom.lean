import Vizagrams.Transformations
import SciLean
import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay

open SciLean Scalar RealScalar

set_option autoImplicit true
set_default_scalar Float

open GeometricTransformation
namespace GeometricPrimitive

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

def line (src : Float^[2] := ⊞[0.0,0.0]) (tgt : Float^[2] := ⊞[1.0,1.0]) := Geom.line src tgt
def circle (r : Float := 1.0) (c : Float^[2] := ⊞[0.0,0.0]) := Geom.circle r c
def polyline (points : Array (Float^[2]) := #[⊞[-1.0,0.0],⊞[0.5,0.5], ⊞[1.0,0.0]]) := Geom.polyline points
def polygon (points : Array (Float^[2]) := #[⊞[-0.5,-0.5],⊞[0.5,-0.5], ⊞[0.5,0.5],⊞[-0.5,0.5]]) := Geom.polygon points

#eval ⊞[1.0,2.0] * 2.0
#eval (G.translate ⊞[1.,2.0]) * Geom.circle 1 ⊞[1,0]
#eval (G.translate ⊞[1.,2.0]) * circle
#eval (G.translate ⊞[1.,2.0]) * polygon
#eval (G.scale 2.0) * polygon
#eval (G.rotate (π / 2)) * polygon
#eval (G.translate ⊞[1.0,0] ∘ G.rotate (π)) * circle
#eval (G.translate ⊞[1.0,0]) * circle
#eval G.translate ⊞[1.0,0] * ⊞[1.,0]

#eval circle
#eval ϕ circle
#eval G.translate ⊞[1.0,0] * (ϕ circle)
private def test := G.translate ⊞[1.0,0] * (ϕ circle)
#eval test
private def get : CovGeom → Float^[2]
  | .circle p1 p2 => (p1 + p2)
  | _ => ⊞[-1,-1]
#eval (get test)

#eval ψ (G.translate ⊞[1.0,0] * (ϕ circle))

#eval ψ (ϕ circle)

end GeometricPrimitive
