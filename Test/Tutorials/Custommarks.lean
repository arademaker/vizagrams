import Vizagrams
-- Working with Graphical Marks

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad
open LinearAlgebra
open Envelope
open Sty

/-
d = Arrow()
draw(d, height=100)
-/

-- Arrow Mark
structure Arrow where
    pts : Vec2 × Vec2
    headsize : Float
    headstyle: Sty.Style
    headmark : Mark

instance : MarkInterface Arrow where
  θ m :=
    let line : 𝕋 Mark := new_line (m.pts.fst) (m.pts.snd)  {stroke_color := Color.mk 1 0 0 ,
                                                            stroke_width := Sty.StyleSize.px 10 }
    let 𝕞 : 𝕋 Mark := envelope_position_marks line (m.pts.snd - m.pts.fst) m.headmark
    flat ( line + 𝕞)

instance : Coe Arrow Mark where
 coe m := Mark.mk m

-- Regular Polygon Mark
structure RegularPolygon where
  center : Vec2 := ![0,0]
  sides : Nat
  size : Float
  h : sides >= 3 := by decide
  style : Sty.Style

instance : ToString RegularPolygon where
  toString p := s!"Regular Polygon with {p.sides} sides of size {p.size}"

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
  θ h := new_polygon (regToPoly h) h.style

instance : Coe RegularPolygon Mark where
  coe m := Mark.mk m

-- Drawing an Arrow with a Triangle as HeadMark
def triangle : RegularPolygon := {center := ![0, 0], sides := 3 , size := 1, style := {fill_color := Color.mk 0 0 1 }}
#html draw₁ triangle

def arrow₁ : 𝕋 Mark :=
  {
    pts := (![0, 0], ![2, 0]),
    headsize := 0.3,
    headstyle := {fill_color := Color.mk 1 0 0},
    headmark := Mark.mk {
      center := ![0, 0],
      sides := 3,
      size := 0.3,
      style := {fill_color := Color.mk 1 0 0}
    : RegularPolygon}
  : Arrow }


#html draw₁ arrow₁

def 𝕋rotate (y : Float) : FreeMonad.ℍ := ℍ.mk {} (rotate y)
#html draw₁ ( 𝕋rotate (π/3) * arrow₁)
def 𝕋scale (y : Float) : ℍ := ℍ.mk {} (scale y)
#html draw₁ (𝕋scale 0.5 * arrow₁)
def 𝕋translate (v : Vec2) : ℍ := ℍ.mk {} (translate v)

-- Face Mark
structure Face where
  center     : Vec2 := ![0, 0]
  size       : Float := 1
  eyestyle   : Style := {}
  headstyle  : Style := {}
  smile      : Float := 0
  smilestyle : Style := {}

instance : MarkInterface Face where
  θ f :=
    -- styles ---------------------------------------------
    let eyeStyle  := { fill_color := some (Color.mk 0 0 1) } ++ f.eyestyle
    let headStyle :=
      { fill_color := some (Color.mk 1 1 1),
        stroke_color := some (Color.mk 0 0 0) } ++ f.headstyle
    let smileStyle := { fill_color := none } ++ f.smilestyle

    -- head -----------------------------------------------
    let head : 𝕋 Mark := new_circle 5 f.center headStyle

    -- eyes -----------------------------------------------
    let eyeOffsetY : Float :=  2
    let eyeOffsetX : Float :=  2
    let leftEyeCenter  : Vec2 := f.center + ![-eyeOffsetX, eyeOffsetY]
    let rightEyeCenter : Vec2 := f.center + ![ eyeOffsetX, eyeOffsetY]
    let eyeLeft  : 𝕋 Mark := new_circle 1 leftEyeCenter  eyeStyle
    let eyeRight : 𝕋 Mark := new_circle 1 rightEyeCenter eyeStyle
    let eyes := eyeLeft + eyeRight

    -------------------------------------------------------
    -- >>> smile (Quadratic Bézier)  <<<
    -------------------------------------------------------
    let moveto : Vec2 := f.center + ![75,250]
    let qbezier : Vec2 × Vec2 := {fst := f.center + ![(f.center 0) + 150, 300 + f.smile], snd := moveto + ![225,0] }
    let smile : 𝕋 Mark := new_qbezier moveto qbezier f.smilestyle
    flat (head + eyes + smile)

instance : Coe Face Mark where
  coe f := Mark.mk f

def face₁ : 𝕋 Mark :=
  { center    := ![5, 5],
    size      := 0.0001,
    smile     := 100.,
    eyestyle  := { fill_color := some (Color.mk 0.5 0.5 1) },
    headstyle := {},
    smilestyle :=
    { stroke_color := Color.mk 0 0 1,
      fill_color := Color.mk 0 1 0,
      stroke_width := Sty.StyleSize.px 5}
  : Face }

#html draw ( (𝕋scale 1) * face₁) --(BoundingBox.toFrame (bounding_box_𝕋 ( face₁)) )


def rectEstilo : Style := { fill_color := some (Color.mk 0.5 0.5 0.5) }
def rect₁ : 𝕋 Mark := new_polygon #[![0,0],![2,0],![2,1],![0,1]] rectEstilo
def rectTransladada : 𝕋 Mark := 𝕋translate (![0, -1.5]) * rect₁

def angles : Array Float :=
  (Array.range 11).map (fun i => i.toFloat * π / 10.0)

def rotatedArrow (a : Float) : 𝕋 Mark :=
  let headTri : RegularPolygon := {
    center := ![0, 0],
    sides  := 3,
    size   := a / 10,
    style  := { fill_color := some (Color.mk 1 0 0) }
  }
  let arr : Arrow := {
    pts       := (![1, 0], ![2, 0]),
    headsize  := a / 10,
    headstyle := { fill_color := some (Color.mk 1 0 0) },
    headmark  := Mark.mk headTri
  }
  𝕋rotate a * (arr : 𝕋 Mark)

def allArrows : 𝕋 Mark :=
  let arrowArray := angles.map rotatedArrow
  if h : arrowArray.size > 0 then
    arrowArray.foldl (fun acc arr => acc + arr) arrowArray[0]
  else
    rotatedArrow 0

def centerCircle : 𝕋 Mark :=
  new_circle 0.5 ![0, 0] { fill_color := some (Color.mk 0 0 1) }

def greyRectangle : 𝕋 Mark :=
  let rectStyle : Style := { fill_color := some (Color.mk 0.5 0.5 0.5) }
  let rect : 𝕋 Mark := new_polygon #[![-1,0],![1,0],![1,1],![-1,1]] rectStyle
  𝕋translate ![0, -1.5] * rect

def finalDrawing : 𝕋 Mark :=
  (centerCircle + greyRectangle)

#html draw₁ ((𝕋scale 0.5 * allArrows) + finalDrawing)

structure Tree_ where
  h : Float
deriving Inhabited

instance : MarkInterface Tree_ where
  θ t :=
    let height := t.h

    let trunkStyle : Style := { fill_color := some (Color.mk 0.6 0.3 0.1) }
    let trunk : 𝕋 Mark :=
      new_polygon #[
        ![-0.25, 0], ![ 0.25, 0],
        ![ 0.25, height/2], ![-0.25, height/2]
      ] trunkStyle

    let leafStyle : Style := { fill_color := some (Color.mk 0   0.8 0) }

    let bigLeaf : 𝕋 Mark :=
      new_circle 0.5 ![0,0] leafStyle

    let angles : Array Float :=
      (Array.range 10).map (fun i => i.toFloat * 0.7)
    let smallLeaves : Array (𝕋 Mark) :=
      angles.map fun θ =>
        𝕋translate (![Float.cos θ * 0.5, Float.sin θ * 0.5])
        * (new_circle 0.3 ![0,0] leafStyle : 𝕋 Mark)

    let leavesTotal : 𝕋 Mark :=
      smallLeaves.foldl (· + ·) bigLeaf

    let leafOff : Float := height/2 + 0.5
    flat (𝕋scale 0.75 * (trunk + ((𝕋translate (![0, leafOff])) * leavesTotal)))

instance : Coe Tree_ Mark where
  coe t := Mark.mk t

def diagram : 𝕋 Mark :=  (Tree_.mk 3) → Tree_.mk 6

#html draw₁ diagram

structure Forest where
  n : Nat

instance : MarkInterface Forest where
  θ f :=
    let half := Float.sqrt (f.n.toFloat) * 0.5 + 1.0

    let lcg (s : UInt32) : UInt32 := s * 1664525 + 1013904223
    let randF (s : UInt32) : Float × UInt32 :=
      let s' := lcg s
      (s'.toFloat / 4294967296.0, s')

    let (_, posArr) := Id.run do
      let mut s : UInt32 := 4
      let mut arr : Array Vec2 := #[]
      for _ in [: f.n] do
        let (x, s1) := randF s; s := s1
        let (y, s2) := randF s; s := s2
        arr := arr.push ![(x * 2 - 1) * half, (y * 2 - 1) * half]
      pure (s, arr)

    let treePrimsArr : Array (Array Prim) :=
      posArr.map fun p =>
        flat (𝕋translate p * (𝕋scale 0.001) * (Tree_.mk 2 : 𝕋 Mark))

    let allTreePrims : Array Prim :=
      treePrimsArr.foldl (· ++ ·) #[]

    let bg : 𝕋 Mark :=
      new_polygon #[
        ![-half, -half], ![ half, -half],
        ![ half,  half], ![-half,  half]
      ] { fill_color := some (Color.mk 0.5 0.5 0.5)}
    let bgPrims := flat bg

    bgPrims ++ allTreePrims

instance : Coe Forest Mark where
  coe f := Mark.mk f

#html draw (Forest.mk 20) (BoundingBox.toFrame (bounding_box_𝕋 (Forest.mk 20)))
