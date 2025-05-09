import Vizagrams.LeannearAlgebra

set_option diagnostics true

namespace GeometricPrimitive

-- Utilizamos uma Inductive pois o conjunto de Primitivas Geométricas é finito
inductive Geom where
  | line     (src trg : Vec2)
  | circle   (r : Float) (c : Vec2)
  | ellipse  (rx ry : Float) (c : Vec2)
  | rect     (corner : Vec2) (width height : Float)
  | polyline (points : Array Vec2)
  | polygon  (points : Array Vec2)
  | path     (d : String)
  | text     (pos : Vec2) (content : String) (size : Float)
deriving Repr

-- É mais fácil trabalhar tansformações geométricas utilizando suas formas covariantes
inductive CovGeom where
  | line     (src trg : Vec2)
  | circle   (p1 p2 : Vec2)           -- dois pontos diametralmente opostos
  | ellipse  (p1 p2 : Vec2)           -- define eixo maior (os dois eixos se inferem)
  | rect     (corner p : Vec2)        -- canto superior esquerdo + canto oposto
  | polyline (points : Array Vec2)
  | polygon  (points : Array Vec2)
  | path     (d : String)             -- não covariante (apenas passa direto)
  | text     (pos : Vec2) (content : String) (size : Float) -- apenas translada
deriving Repr

def ϕ : Geom → CovGeom
  | .line src trg          => .line src trg
  | .circle r c            => .circle (c - ![r, 0]) (c + ![r, 0])
  | .ellipse rx ry c       => .ellipse (c - ![rx, 0]) (c + ![rx, 0])  -- assume eixo horizontal
  | .rect corner w h       => .rect corner (corner + ![w, h])
  | .polyline points       => .polyline points
  | .polygon points        => .polygon points
  | .path d                => .path d
  | .text pos content size => .text pos content size

def ψ : CovGeom → Geom
  | .line src trg          => .line src trg
  | .circle p1 p2          =>
      let r := Vec2Norm (p1 - p2) / 2
      let c := ScalarMul 0.5 (p1 + p2)
      .circle r c
  | .ellipse p1 p2         =>
      let rx := Vec2Norm (p1 - p2) / 2
      let c  := ScalarMul 0.5 (p1 + p2)
      .ellipse rx rx c
  | .rect corner p         =>
      let wh := p - corner
      .rect corner (wh 0) (wh 1)
  | .polyline points       => .polyline points
  | .polygon points        => .polygon points
  | .path d                => .path d
  | .text pos content size => .text pos content size

instance : HMul Mat2Vec2 CovGeom CovGeom where
  hMul g p := match p with
    | .line src tgt      => .line (g ⬝ src) (g ⬝ tgt)
    | .circle p1 p2      => .circle (g ⬝ p1) (g ⬝ p2)
    | .ellipse p1 p2     => .ellipse (g ⬝ p1) (g ⬝ p2)
    | .rect c p          => .rect (g ⬝ c) (g ⬝ p)
    | .polyline points   => .polyline (points.map (g ⬝ ·))
    | .polygon points    => .polygon (points.map (g ⬝ ·))
    | .path d            => .path d
    | .text pos txt sz   => .text (g ⬝ pos) txt sz

instance : HMul Mat2Vec2 Geom Geom where
  hMul g p := ψ (g * (ϕ p))

end GeometricPrimitive
