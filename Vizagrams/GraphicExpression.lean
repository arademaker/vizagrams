import Vizagrams.FreeMonad
import Vizagrams.VizBackend

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad


/-!
# Graphic Expressions — Hylomorphism Pattern

This module defines `GraphicExpression`, a data-driven visualization abstraction based on
the categorical **hylomorphism** pattern.

## Categorical Background

Let 𝓒 be a category and 𝓕 : 𝓒 → 𝓒 an endofunctor.

- An **F-algebra** is a pair `(A, g)` where `A ∈ 𝓒` (carrier) and `g : 𝓕 A → A` (structure map).
- An **F-coalgebra** is a pair `(U, h)` where `h : U → 𝓕 U`.
- A **hylomorphism** fuses a coalgebra (anamorphism, "unfold") with an algebra (catamorphism, "fold"):
  ```
  hylo(alg, coalg) = alg ∘ F(hylo) ∘ coalg
  ```

## Graphic Expression

A `GraphicExpression` specialises the hylomorphism pattern to visualization.
It is a triple `(expr, alg, coalg)` over a data type `D` where the functor is `List`:

```
expr   : D → 𝕋 Mark          -- render a single datum
alg    : List (𝕋 Mark) → 𝕋 Mark  -- combine rendered children
coalg  : D → List D           -- decompose a datum into sub-data
```

Evaluation is then:
```
eval = alg ∘ map expr ∘ coalg
```
-/

/--
A data-driven graphic: packages a single-datum renderer (`expr`), a combining algebra
(`alg`), and a decomposition coalgebra (`coalg`) into a reusable visualization pattern.
-/
structure GraphicExpression (α : Type) where
  expr  : α → 𝕋 Mark
  alg   : List (𝕋 Mark) → 𝕋 Mark
  coalg : α → List α

/--
Evaluate a `GraphicExpression` on input `a`: decompose with `coalg`, render each piece
with `expr`, then combine with `alg`.
-/
def GraphicExpression.eval {α : Type} (ge : GraphicExpression α) : α → 𝕋 Mark :=
  ge.alg ∘ (List.map ge.expr) ∘ ge.coalg

/-
def table : Matrix (Fin 4) (Fin 3) Float := !![ 0, 1.2, 2.1
                                              ; 0, 1 , 3
                                              ; 1, 1, 1
                                              ; 1 , 1 ,1]

#eval (table 0 2) - (table 0 1)

def Expr₁ (τ : Matrix (Fin 1) (Fin 3) Float) : 𝕋 Mark :=
  let color :=
    if ( τ 0 0 ) <= 0.5 then ( Color.mk 1 0 0 ) else ( Color.mk 1 0.5 0.9)
  let center : Vec2 := ![ τ 0 1 , τ 0 2 ]
  let Circle : 𝕋 Mark := 𝕋Circle 0.5 center {fill_color := color}
  Circle

def coalg₁ (τ : Matrix (Fin 4) (Fin 3) Float) : List (Matrix (Fin 1) (Fin 3) Float) :=
  List.ofFn (λ i : Fin 4 => Matrix.of ![τ i])

#check (List.foldr (fun acc x => acc + x) (List.map Expr₁ (coalg₁ table) ))

def alg₁ : List (𝕋 Mark) → 𝕋 Mark
  | []      => 𝕋.pure (NewCircle 0 ![0,0])  -- ou algum outro marcador vazio
  | (x::xs) => List.foldl (· + ·) x xs

def evaluate := alg₁ ∘ (List.map Expr₁) ∘ coalg₁
#check evaluate table
#html draw (evaluate table)


def data : List Float :=  [1.0, 2.3, 0.7, 3.1, 0.3]

def coalgbar (τ : List Float) : List ( List Float ) :=
  τ.map (fun x => [x])

def barPolygon (h : Float) (w : Float := 0.8) : Prim :=
  let pts : Array Vec2 :=
    #[![0, 0], ![w, 0], ![w, h], ![0, h]]
  NewPolygon pts {fill_color := Color.mk (3*h/2) (2*h/3) (h/5) }

def barExpr : List Float → 𝕋 Mark
  | [h] => 𝕋.pure (barPolygon h)
  | _   => 𝕋.pure (NewCircle 0 ![0,0])

def algBar (τ : List (𝕋 Mark)) : 𝕋 Mark :=
  match τ with
  | x :: xs => List.foldl (· →[0.5] ·) x xs
  | [] => 𝕋.pure (NewCircle 0 ![0,0])

def bars : GraphicExpression (List Float):= {
  expr := barExpr
  coalg := coalgbar
  alg := algBar
}
#check bars.eval data

#html draw ( bars.eval data)
-/
