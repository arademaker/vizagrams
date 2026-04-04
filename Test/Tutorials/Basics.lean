import Vizagrams

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad
open LinearAlgebra
open Envelope

-- commit version 5da23d52696673c7b487253bb427959bdf2aa413 Vizagrams.jl
-- 1. Drawing a simple diagram

/-
d = Circle()
draw(d)
-/
def circle₁ : 𝕋 Mark := new_circle

#html draw₁ circle₁

-- 2. Applying Transformations

/-

d = Square()
draw(d,height=100)
-/
def square₁ : 𝕋 Mark := new_polygon #[![-1,-1], ![-1,1], ![1,1], ![1,-1]]

#html draw₁ square₁

/-
d = R(π/4)*Square()
draw(d,height=100)
-/
def rotate45 : ℍ := ℍ.mk {} (rotate (π/4))

#html draw₁ (rotate45 * square₁)
/-
d = S(:fill=>:red,:stroke=>:blue,:stroke_width=>0.1)R(π/4)*Square()
draw(d,height=100)
-/
def toRed : ℍ := ℍ.mk {fill_color := Color.mk 1 0 0 , stroke_color := Color.mk 0 0 1, stroke_width := Sty.StyleSize.px 4 } (scale 1)

#html draw₁ (toRed * square₁)

#html draw₁ ( toRed * ( rotate45 * square₁))

-- 3. Combining Marks for more complex Diagrams

/-
d = S(:fill=>:blue)Circle() + S(:fill=>:red)RegularPolygon(n=3) +S(:fill=>:green)Square()
draw(d,height=100)
-/
def circleblue : 𝕋 Mark := new_circle 1 ![0,0] { fill_color := Color.mk 0 0 1 }
def squaregreen : 𝕋 Mark := ({s := {} , g := scale 0.5}:ℍ) * (new_polygon #[![-1,-1], ![-1,1], ![1,1], ![1,-1]] {fill_color := Color.mk 0 1 0}: 𝕋 Mark)
def trianglered : 𝕋 Mark := ({s := {} , g := scale 1}:ℍ) * (new_polygon #[![-0.85,-0.5],![0.85,-0.5],![0,1]] {fill_color := Color.mk 1 0 0}: 𝕋 Mark)

#html draw₁ (circleblue + trianglered + squaregreen)
/-
d = T(3,0)Circle() + T(0,2)RegularPolygon(n=3) +R(π/10)U(2)Square()
draw(d,height=100)
-/
def t3x : ℍ := ℍ.mk {} ( translate ![3,0])
def t2y : ℍ := ℍ.mk {} ( translate ![0,2])
def rs : ℍ := ℍ.mk {} (rotate (π/10) ∘ₜ scale 2)

#html draw₁ ( (t3x * circle₁) + (t2y * trianglered) + (rs * squaregreen))

/-
d = S(:fill=>:blue)*(T(3,0)Circle() + T(0,2)RegularPolygon(n=3) +R(π/10)U(2)Square()) + T(2,-2)S(:stroke=>:red,:stroke_width=>0.5)Line([[0,0],[3,0],[3,3]])
draw(d,height=100)
-/

def line₁ : 𝕋 Mark := ( {geom := .polyline #[ ![0,0], ![3,0], ![3,3]], style := {stroke_color := Color.mk 1 0 0, stroke_width :=  Sty.StyleSize.px 20}} : Prim)

#html draw₁ (((t3x * circle₁) + (t2y * trianglered) + (rs * squaregreen) ) + (((translate ![2,-2]): ℍ) * line₁ ))

-- 4. Combining Diagrams

/-
d1 = Circle() + T(2,0)*Circle()
d2 = S(:fill=>:blue)*(T(3,0)Circle() + T(0,2)RegularPolygon(n=3) +R(π/10)U(2)Square())
d  = d1 + T(5,0)R(π/5)*d2

draw(d,height=100)
-/
def diagram₁ : 𝕋 Mark := circle₁ + (translate ![2,0]: ℍ)*circle₁
def diagram₂ : 𝕋 Mark := (t3x * circle₁) + (t2y * trianglered) + (rs * squaregreen)

#html draw₁ (diagram₁ + (translate ![5,0] : ℍ) * diagram₂)

-- 5. Stacking Diagrams

/-
d = Circle() → Circle() ↑ Square()
draw(d,height=100)
-/
#html draw₁ (circle₁ → circle₁ ↑ square₁)

/-
d = Circle() → Circle() ↑ Square()
draw(d,height=100)
-/
#html draw₁ (circle₁ →[0.5] circle₁ →[0.5] circle₁)

/-
d = mapreduce(c->Circle(), (x,y)-> x → (T(0.5,0),y), 1:5)
draw(d,height=100)
-/

def shiftedChain : 𝕋 Mark :=
  (List.range 5).map (fun _ => circle₁)
    |>.foldr (fun y x => x → (translate ![0.5, 0] : ℍ) * y) circle₁

#html draw₁ shiftedChain

-- 6. Text and LaTeX

/-
d = S(:fontFamily=>"Futura")TextMark(text="My Text",anchor=:c,fontsize=1)
draw(d,height=100)
-/

def text : 𝕋 Mark := new_text "My Text"

#html draw₁ text

/-
cross = S(:stroke_width=>0.1)*(Line([[-3,0],[3,0]]) + R(π/2)Line([[-3,0],[3,0]]))

anchors = [:c, :s, :n, :e, :w, :se, :sw, :ne, :nw]
d = mapreduce(a->cross + TextMark(text=uppercase("$a"),anchor=a,fontsize=1),(x,y)-> x → (T(0.5,0),y),anchors)

draw(d,height=80)
-/

/-
using LaTeXStrings
d = S(:fill=>:blue)TextMark(text=L"e^x = \sum^\infty_{i=0}\frac{x^n}{n!} = 1 + x + \frac{x^2}{2} + \cdots")
draw(d,height=100)
-/
def exp_x : 𝕋 Mark := new_text "eˣ = ∑ xⁿ/n! = 1 + x + x²/2 + ... " ![-2,0] 0.75

#html draw₁ exp_x
