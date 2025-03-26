import Vizagrams.VizPrim
import Vizagrams.VizBackend
import Vizagrams.VizMark

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg

--set_option diagnostics true
-- Creating a Circle

def MyCircle (Radius:Float:= 1.0 ) (Center : Float^[2] := ⊞[ 0.0 , 0.0 ]): Geom := Geom.circle Radius  Center

#eval MyCircle

def x : Prim := {geom := MyCircle, s := {fillColor := Color.mk 1 1 0}}

#html drawsvg #[x]

def LazyCircle (r := 1.0) (c := ⊞[1.0,-1.0] ) := Geom.circle r c
#eval LazyCircle
-- A lot of work to draw a circle

-- New form to draw a circle
def EasyCircle := NewCircle 1.2 ⊞[0.5,0.25]
#check EasyCircle
#html drawsvg #[EasyCircle]

def EasyCircleY := NewCircle (r:=1.2) (c:= ⊞[0.5,0.25]) (st := {fillColor := Color.mk 1 5 0})
#html drawsvg #[EasyCircleY]


-- Creating Line
def MyLine (p₁ := ⊞[0.0,0.0] ) (p₂ := ⊞[1.0,1.0]) := Geom.line p₁ p₂
#eval MyLine

def y : Prim := {geom:= MyLine, s := {strokeColor:= Color.mk 1 1 2}}
#check y
#html drawsvg #[y]

-- Triangle
def MyPolygon (pts := #[⊞[-0.7,-0.5],⊞[0.7,-0.5],⊞[0.5,0.5]] ) := Geom.polygon pts
def z : Prim := {geom := MyPolygon, s :={fillColor := Color.mk 0 1 1}}
#html drawsvg #[z]

def EasyPolygon := NewPolygon #[⊞[-0.7,-0.5],⊞[0.7,-0.5],⊞[0.5,0.5]]

#html drawsvg #[EasyPolygon]

def EasyLine := NewLine ⊞[-0.7,-0.5] ⊞[0.7,-0.5]
#check EasyLine
#html drawsvg EasyLine

def translationrigth := GeometricTransformation.G.translate ⊞[1,0]

def a := translationrigth * EasyPolygon
#html drawsvg #[a]
def b := (GeometricTransformation.G.rotate 3.14) * EasyPolygon
#html drawsvg #[b]

/-Developing head mark -/
open GraphicalMark

structure Head where
  size : Float
  smile : Float
deriving Repr

instance : ToString Head where
  toString h := "Head (size: " ++ toString h.size ++ ", smile: " ++ toString h.smile ++ ")"

instance : MarkInterface Head where
  θ h :=
    let eyes := (NewCircle 0.3 ⊞[-0.8,1] {fillColor := Color.mk 0 1 1 }) ⊕ (NewCircle 0.3 ⊞[0.8,1] {fillColor := Color.mk 0 1 1 })
    let smile : Prim := { geom := Geom.line ⊞[-1,-0.5] ⊞[1,-0.5], s := {strokeColor:= Color.mk 1 1 2} }
    let head := NewCircle (2*h.size) ⊞[0,0]
    head ⊕ eyes ⊕ smile

def head_o : Head := Head.mk 1 2
def head_m : Mark := Mark.mk head_o
#eval head_m


#html drawsvg (head_m ⊕ a)
