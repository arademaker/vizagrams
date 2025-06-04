import Vizagrams
-- Working with Graphical Marks

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad
open Envelope

-- Arrow
structure Arrow where
    pts : Vec2 × Vec2
    headsize : Float
    headstyle: Sty.Style
    headmark : Mark

instance : MarkInterface Arrow where
  θ m :=
    let line : 𝕋 Mark := NewLine (m.pts.fst) (m.pts.snd)  {strokeColor := Color.mk 1 0 0 ,
                                                            strokeWidth := Sty.StyleSize.px 10 }
    let 𝕞 : 𝕋 Mark := envelopePositionMarks line (m.pts.snd - m.pts.fst) m.headmark
    flat ( line + 𝕞)

instance : Coe Arrow Mark where
 coe m := Mark.mk m

structure RegularPolygon where
  center : Vec2 := ![0,0]
  sides : Nat
  size : Float
  h : sides >= 3 := by decide
  style : Sty.Style

instance : ToString RegularPolygon where
  toString p := s!"Regular Polygon with {p.sides} sides of size {p.size}"

-- Agora precisamos implementar MarkInterface
def findPointbyAngle (x : Float) : Vec2 :=
  ![Float.cos x , Float.sin x]

def create_list (n : ℕ) : Array ℕ :=
  Array.range n |>.map (λ x => x + 1)

def multiply_by_scalar (lst : Array ℕ) (scalar : Float) : Array Float :=
  lst.map (λ x => ↑x.toFloat * scalar)

def regToPoly (p : RegularPolygon) : Array (Vec2) :=
  let passo : Float := ( 2 * π  ) / (p.sides.toFloat)
  let listn : Array ℕ := create_list (p.sides)
  let Arr := multiply_by_scalar listn passo
  Array.map findPointbyAngle Arr

instance : MarkInterface RegularPolygon where
  θ h := NewPolygon (regToPoly h) h.style

instance : Coe RegularPolygon Mark where
  coe m := Mark.mk m

def triangle : RegularPolygon := {sides := 3 , size := 1, style := {fillColor := Color.mk 0 0 1 }}
#html draw triangle

def arrow₁ : 𝕋 Mark :=
  {
    pts := (![0, 0], ![2, 0]),
    headsize := 0.3,
    headstyle := {fillColor := Color.mk 1 0 0},  -- vermelho
    headmark := Mark.mk {
      center := ![0, 0],
      sides := 3,
      size := 0.3,
      style := {fillColor := Color.mk 1 0 0}
    : RegularPolygon}
  : Arrow }

def 𝕋rotate (y : Float) : FreeMonad.ℍ := { s := {} , g := rotate y}

#html draw ( 𝕋rotate (π/3) * arrow₁)

open Mathlib
open ProofWidgets.Svg
open Sty

structure Face where
  center     : Vec2 := ![0, 0]
  size       : Float := 1
  eyestyle   : Style := {}
  headstyle  : Style := {}
  smile      : Float := 0
  smilestyle : Style := {}

/--
  NewQBezier: constrói um primitivo de Curva de Bézier Quadrática.

  `bpts`  = lista de pontos de base (Array Vec2), tamanho ≥ 2
  `cpts`  = lista de pontos de controle (Array Vec2), tamanho = bpts.size - 1
  `st`    = estilo (Stroke, Fill, etc). Por padrão, usa linha preta.

  Internamente, esse primitivo corresponde a `Geom.qbezier bpts cpts` em `Vizagrams.Geom`,
  e será convertido para SVG no `geomToShape`.
-/
def NewQBezier
  (bpts : Array Vec2)
  (cpts : Array Vec2)
  (st   : Style := { strokeColor := Color.mk 1 0 0 ,strokeWidth := Sty.StyleSize.px 50})
  : Prim :=
  let q := Geom.qbezier bpts cpts
  { geom := q, style := st }


instance : MarkInterface Face where
  θ f :=
    let eyeStyle  := Style.comp { fillColor := some (Color.mk 0 0 1) } f.eyestyle
    let headStyle := Style.comp
      { fillColor := some (Color.mk 1 1 1),
        strokeColor := some (Color.mk 0 0 0) } f.headstyle
    let smileStyle := Style.comp { fillColor := none } f.smilestyle

    let head : 𝕋 Mark := NewCircle 5 f.center headStyle

    let eyeOffsetY : Float :=  2
    let eyeOffsetX : Float :=  2
    let leftEyeCenter  : Vec2 := f.center + ![-eyeOffsetX, eyeOffsetY]
    let rightEyeCenter : Vec2 := f.center + ![ eyeOffsetX, eyeOffsetY]
    let eyeLeft  : 𝕋 Mark := NewCircle 1 leftEyeCenter  eyeStyle
    let eyeRight : 𝕋 Mark := NewCircle 1 rightEyeCenter eyeStyle
    let eyes := eyeLeft + eyeRight

    let bptsOrig : Array Vec2 := #[ ![-1.3, 0], ![1.3, 0] ]
    let cptsOrig : Array Vec2 := #[ ![0, -f.smile] ]

    let bptsScaled : Array Vec2 := bptsOrig.map (fun v => ScalarMul 2 v)
    let cptsScaled : Array Vec2 := cptsOrig.map (fun v => ScalarMul 2 v)

    let offsetSmile : Vec2 := ScalarMul f.smile (![0, 1])
    let bptsSmile  : Array Vec2 := bptsScaled.map (fun v => v + offsetSmile)
    let cptsSmile  : Array Vec2 := cptsScaled.map (fun v => v + offsetSmile)

    let smilePos : Vec2 := ![0, -1.5]
    let bptsFinal : Array Vec2 := bptsSmile.map (fun v => v + smilePos)
    let cptsFinal : Array Vec2 := cptsSmile.map (fun v => v + smilePos)

    let bptsCentered : Array Vec2 := bptsFinal.map (fun v => v + f.center)
    let cptsCentered : Array Vec2 := cptsFinal.map (fun v => v + f.center)

    let fator : Float := f.size / 5
    let bptsOut : Array Vec2 := bptsCentered.map (fun v => ScalarMul fator v)
    let cptsOut : Array Vec2 := cptsCentered.map (fun v => ScalarMul fator v)

    let smile : 𝕋 Mark := NewQBezier bptsOut cptsOut smileStyle

    flat (head + eyes + smile)

instance : Coe Face Mark where
  coe f := Mark.mk f

def face₁ : 𝕋 Mark :=
  { center    := ![2, 2],
    size      := 0.0001,
    smile     := 0.08,
    eyestyle  := { fillColor := some (Color.mk 0.5 0.5 1) },
    headstyle := {},
    smilestyle := { strokeColor := some (Color.mk 1 0 0) , strokeWidth := Sty.StyleSize.px 50},
  : Face }

def 𝕋scale (z : Float) : FreeMonad.ℍ := { s := {} , g := scale z}

#html draw ( (𝕋scale 0.5) * face₁)
