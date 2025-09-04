/-
Copyright (c) 2025 Henrique Borges. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Davi Barreira, Henrique Borges
-/
import Vizagrams.LeannearAlgebra
/-!
# Geometric Primitives for Vizagrams

This module defines the geometric primitives used in the Vizagrams library.
It introduces two key representations for geometric shapes:
- `Geom`: A **semantic** representation, which is intuitive and easy for users to construct
  (e.g., a circle is defined by its center and radius).
- `CovGeom`: A **covariant** representation, where shapes are defined entirely by points (`Vec2`).
  This form is mathematically elegant and makes applying affine transformations trivial.

The module provides functions to convert between these two representations and defines the action
of affine transformations on both types.
-/

namespace GeometricPrimitive

open LinearAlgebra

/--
A semantic representation of a geometric primitive.
This form is easy for users to construct and reason about.
-/
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
  | qbezier  (Moveto : Vec2) (QbezierCurveto : Vec2 × Vec2)
  | cbezier  (Moveto : Vec2) (CbezierCurveto : Vec2 × Vec2 × Vec2)
deriving Repr

/--
A covariant representation of a geometric primitive.
All components are points (`Vec2`), which transform uniformly ("covariantly").
This form is ideal for applying transformations.
-/
inductive CovGeom where
  | line     (src trg : Vec2)
  | circle   (p1 p2 : Vec2)
  | ellipse (center pAxisX pAxisY : Vec2)
  | rect     (corner p : Vec2)
  | polyline (points : Array Vec2)
  | polygon  (points : Array Vec2)
  | path     (d : String)
  | text     (pos : Vec2) (content : String) (size : Float)
  | arc      (p1 p2 c p4 p5 : Vec2)
  | qbezier  (Moveto : Vec2) (QbezierCurveto : Vec2 × Vec2)
  | cbezier  (Moveto : Vec2) (CbezierCurveto : Vec2 × Vec2 × Vec2)
deriving Repr

/-- Converts a semantic `Geom` to its covariant representation `CovGeom`. -/
def ϕ : Geom → CovGeom
  | .line src trg          => .line src trg
  | .circle r c            => .circle (c - ![r, 0]) (c + ![r, 0])
  | .ellipse rx ry c =>
    let pAxisX := c + ![rx, 0.0]
    let pAxisY := c + ![0.0, ry]
    .ellipse c pAxisX pAxisY  | .rect corner w h       => .rect corner (corner + ![w, h])
  | .polyline points       => .polyline points
  | .polygon points        => .polygon points
  | .path d                => .path d
  | .text pos content size => .text pos content size
  | .arc rx ry c rot i f     =>
      let p1 := rotateVec2 (![rx, 0]) rot + c
      let p2 := rotateVec2 (![0, ry]) rot + c
      let p4 := rotateVec2 (pointOnEllipse i rx ry) rot + c
      let p5 := rotateVec2 (pointOnEllipse f rx ry) rot + c
      .arc p1 p2 c p4 p5
  | .qbezier m cv          => .qbezier m cv
  | .cbezier bpts cpts     => .cbezier bpts cpts

/-- Converts a covariant `CovGeom` back to its semantic representation `Geom`. -/
def ψ : CovGeom → Geom
  | .line src trg          => .line src trg
  | .circle p1 p2          =>
      let r := ‖ (p1 - p2)‖ / 2
      let c := 0.5 • (p1 + p2)
      .circle r c
  | .ellipse center pAxisX pAxisY =>
      let rx := ‖(pAxisX - center)‖
      let ry := ‖(pAxisY - center)‖
    .ellipse rx ry center
  | .rect corner p         =>
      let wh := p - corner
      .rect corner (wh 0) (wh 1)
  | .polyline points       => .polyline points
  | .polygon points        => .polygon points
  | .path d                => .path d
  | .text pos content size => .text pos content size
  | .arc p1 p2 c p4 p5       =>
      let rx := ‖ (p1 - c) ‖;
      let ry := ‖ (p2 - c) ‖;
      let rot := atan2pi (p1 - c)
      let v1 := rotateVec2 (p4 - c) (-rot)
      let i := atan2pi ![v1 0 / rx, v1 1 / ry]
      let v2 := rotateVec2 (p5 - c) (-rot)
      let f := atan2pi ![v2 0 / rx, v2 1 / ry]
      .arc rx ry c rot i f
  | .qbezier m cv          => .qbezier m cv
  | .cbezier bpts cpts     => .cbezier bpts cpts

/-- Defines the action of a `Mat2Vec2` transformation on a `CovGeom`. -/
instance : HMul Mat2Vec2 CovGeom CovGeom where
  hMul g p := match p with
    | .line src tgt      => .line (g ▷ src) (g ▷ tgt)
    | .circle p1 p2      => .circle (g ▷ p1) (g ▷ p2)
    | .ellipse center pAxisX pAxisY =>
      .ellipse (g ▷ center) (g ▷ pAxisX) (g ▷ pAxisY)
    | .rect c p          => .rect (g ▷ c) (g ▷ p)
    | .polyline points   => .polyline (points.map (g ▷ ·))
    | .polygon points    => .polygon (points.map (g ▷ ·))
    | .path d            => .path d
    | .text pos txt sz   => .text (g ▷ pos) txt sz
    | .arc p1 p2 c p4 p5 => .arc (g ▷ p1) (g ▷ p2) (g ▷ c) (g ▷ p4) (g ▷ p5)
    | .qbezier m (cp, trg) =>
        .qbezier (g ▷ m) (g ▷ cp, g ▷ trg)
    | .cbezier m (p1, p2, p3) =>
        .cbezier (g ▷ m) (g ▷ p1, g ▷ p2, g ▷ p3)

/--
Defines the action of a `Mat2Vec2` transformation on a `Geom`.
This is done by converting to `CovGeom`, applying the transformation, and converting back.
-/
instance : HMul Mat2Vec2 Geom Geom where
  hMul g p := ψ (g * (ϕ p))

end GeometricPrimitive
