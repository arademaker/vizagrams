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
  | arc      (rx ry : Float) (c : Vec2) (rot init final : Float)
  | qbezier (bpts cpts : Array Vec2)
  | cbezier (bpts cpts : Array Vec2)
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
  | arc      (p1 p2 c p4 p5 : Vec2) -- eixo rx, eixo ry, centro, ponto inicial, ponto final
  | qbezier  (bpts cpts : Array Vec2)
  | cbezier  (bpts cpts : Array Vec2)
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
  | .arc rx ry c rot i f     =>
      let p1 := rotateVec (![rx, 0]) rot + c
      let p2 := rotateVec (![0, ry]) rot + c
      let p4 := rotateVec (pointOnEllipse i rx ry) rot + c
      let p5 := rotateVec (pointOnEllipse f rx ry) rot + c
      .arc p1 p2 c p4 p5
  | .qbezier bpts cpts     => .qbezier bpts cpts
  | .cbezier bpts cpts     => .cbezier bpts cpts

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
  | .arc p1 p2 c p4 p5       =>
      let rx := Vec2Norm (p1 - c)
      let ry := Vec2Norm (p2 - c)
      let rot := atan2pi (p1 - c)
      let v1 := rotateVec (p4 - c) (-rot)
      let i := atan2pi ![v1 0 / rx, v1 1 / ry]
      let v2 := rotateVec (p5 - c) (-rot)
      let f := atan2pi ![v2 0 / rx, v2 1 / ry]
      .arc rx ry c rot i f
  | .qbezier bpts cpts     => .qbezier bpts cpts
  | .cbezier bpts cpts     => .cbezier bpts cpts

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
    | .arc p1 p2 c p4 p5 => .arc (g ⬝ p1) (g ⬝ p2) (g ⬝ c) (g ⬝ p4) (g ⬝ p5)
    | .qbezier b c       => .qbezier (b.map (g ⬝ ·)) (c.map (g ⬝ ·))
    | .cbezier b c       => .cbezier (b.map (g ⬝ ·)) (c.map (g ⬝ ·))

instance : HMul Mat2Vec2 Geom Geom where
  hMul g p := ψ (g * (ϕ p))

end GeometricPrimitive
