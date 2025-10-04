import Vizagrams

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad
open LinearAlgebra
-- Example: Drawing a "BarChart"
def data₁ : List Float :=  [1.0, 2.3, 0.7, 3.1, 0.3]

def barPolygon (h : Float) (w : Float := 0.8) : Prim :=
  let pts : Array Vec2 :=
    #[![0, 0], ![w, 0], ![w, h], ![0, h]]
  NewPolygon pts {fillColor := Color.mk (3*h/2) (2*h/3) (h/5) }

def coalgbar (τ : List Float) : List ( List Float ) :=
  τ.map (fun x => [x])

def barExpr : List Float → 𝕋 Mark
  | [h] => 𝕋.pure (barPolygon h)
  | _   => 𝕋.pure (Nil.mk)

def algBar (τ : List (𝕋 Mark)) : 𝕋 Mark :=
  match τ with
  | x :: xs => List.foldl (· →[0.5] ·) x xs
  | [] => 𝕋.pure Nil.mk

def bars : GraphicExpression (List Float):= {
  expr := barExpr
  coalg := coalgbar
  alg := algBar
}

#html draw₁ ( bars.eval data₁)
