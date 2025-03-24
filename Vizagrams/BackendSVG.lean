import Vizagrams.Primitivesorg
import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay
import Scilean

set_option autoImplicit true
set_default_scalar Float

namespace backendsvg

class PrimInterface (α : Type) where
  draw : α → (fr : ProofWidgets.Svg.Frame) → ProofWidgets.Svg.Element fr

structure Prim where
  {T : Type}
  [inst : PrimInterface T]
  [strg : ToString T]
  val : T

class HPlus (α : Type u) (β : Type v) where
  hPlus : α → β → Array Prim

infixr:80 " ⊕ " => HPlus.hPlus

open ProofWidgets Svg in

def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400

open Primitives

/- Circle backend -/

instance : ToString Circle where
  toString c := "Circle (r: " ++ toString c.r ++ ", c: " ++ toString c.c ++ ")"

open ProofWidgets Svg in
def drawCircle (c : Circle) (fr : Frame) : Element fr :=
  circle (c.c[2],c.c[1]) (.abs c.r) |>.setStroke (0.,0.,0.) (.px 2) |>.setFill (0.,1.,1.) |>.setId "point1"

instance : PrimInterface Circle where
  draw := drawCircle

/- Line Backend-/

instance : ToString Line where
  toString l := "Line (l: " ++ toString l.pts ++ ")"

open ProofWidgets Svg in
def drawLine (l : Line) (fr : Frame) : Element fr :=
  line (l.pts.fst[2],l.pts.fst[1]) (l.pts.snd[2],l.pts.snd[1]) |>.setStroke (1.,0.,0.) (.px 2)

def line0 : Line := ⟨ ⊞[0,0] , ⊞[1, 1]⟩
#eval line0
#check (line0.pts.fst[2], line0.pts.fst[1])
open ProofWidgets Svg in
#check line (line0.pts.fst[2], line0.pts.fst[1]) (line0.pts.snd[2],line0.pts.snd[1])

instance : PrimInterface Line where
  draw := drawLine

/- Ellipse Backend -/

instance : ToString Ellipse where
  toString k := s!" Ellipse ( rx: {k.rx}, ry: {k.ry}, c: {k.c}, ang: {k.ang} )"

/- Polygon Backend-/
instance : ToString Polygon where
  toString p := s!"Polygon ( pts: {p.pts})"

def arrayOfPoints : Array (Float^[2]) := #[⊞[1.0, 2.0], ⊞[3.0, 4.0], ⊞[5.0, 6.0]]

open ProofWidgets Svg in
def vecToPoint (x : Float^[2]) (fr : Frame := fr) : Point fr :=
  Point.abs x[0] x[1]

open ProofWidgets Svg in
def drawPolygon (p : Polygon) (fr : Frame) : Element fr :=
  let arr := p.pts
  let arrpts := arr.map (vecToPoint)
  polygon (arrpts) |>.setStroke (1.,0.,0.) (.px 2)

instance : PrimInterface Polygon where
  draw := drawPolygon

/-Develop Prim -/

def Prim.draw (p : Prim) (fr : ProofWidgets.Svg.Frame) : ProofWidgets.Svg.Element fr :=
  p.inst.draw p.val fr

instance : ToString Prim where
  toString p := @ToString.toString p.T p.strg p.val

def prim {α : Type} [PrimInterface α] [ToString α] (a : α) : Prim := Prim.mk a

open ProofWidgets Svg in
def drawsvg (a : Array Prim) (fr : Frame := frame) : ProofWidgets.Html :=
  let svg : ProofWidgets.Svg fr := { elements := Array.map (λx => Prim.draw x fr) a}
  svg.toHtml

/- Develop Class HPlus-/
instance  {α β : Type} [PrimInterface α] [PrimInterface β] [ToString α] [ToString β] : HPlus  α β where
  hPlus p1 p2 := #[prim p1, prim p2]
instance  {α : Type} [PrimInterface α] [ToString α] : HPlus  α (Array Prim) where
  hPlus p a := #[prim p] ++ a
instance  {α : Type} [PrimInterface α] [ToString α] : HPlus  (Array Prim) α where
  hPlus a p := a ++ #[prim p]
instance : HPlus  Prim Prim where
  hPlus p1 p2 := #[p1, p2]
instance  : HPlus  Prim (Array Prim) where
  hPlus p1 p2 := #[p1] ++ p2
instance  : HPlus  (Array Prim) Prim where
  hPlus p1 p2 := p1 ++ #[p2]
instance  : HPlus  (Array Prim) (Array Prim) where
  hPlus p1 p2 := p1 ++ p2

-- Mark backend



end backendsvg
