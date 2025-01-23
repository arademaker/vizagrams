import SciLean
import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay

set_option autoImplicit true
set_default_scalar Float

namespace Primitives

class PrimInterface (α : Type) where
  draw : α → (fr : ProofWidgets.Svg.Frame) → ProofWidgets.Svg.Element fr

structure Circle where
  r : Float
  c : Float^[2]
instance : ToString Circle where
  toString c := "Circle (r: " ++ toString c.r ++ ", c: " ++ toString c.c ++ ")"
def Circle.o : Circle := Circle.mk 1 ⊞[0,0]

open ProofWidgets Svg in
def drawCircle (c : Circle) (fr : Frame) : Element fr :=
  circle (c.c[2],c.c[1]) (.abs c.r) |>.setStroke (0.,0.,0.) (.px 2) |>.setFill (0.,1.,1.) |>.setId "point1"
#eval Circle.o
instance : PrimInterface Circle where
  draw := drawCircle

structure Line where
  pts : (Float^[2]) × (Float^[2])
instance : ToString Line where
  toString l := "Line (l: " ++ toString l.pts ++ ")"
def Line.o : Line := Line.mk (⊞[0,0], ⊞[1,1])
open ProofWidgets Svg in
def drawLine (l : Line) (fr : Frame) : Element fr :=
  line (l.pts.fst[2],l.pts.fst[1]) (l.pts.snd[2],l.pts.snd[1]) |>.setStroke (1.,0.,0.) (.px 2)
#eval Line.o
instance : PrimInterface Line where
  draw := drawLine

open ProofWidgets Svg in
private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400
private def x := @ProofWidgets.Svg.line frame (0.,0.) (1.,0.)



-- #check PrimInterface.mk Circle.o

#check (PrimInterface.draw)
def d := PrimInterface.draw Circle.o frame

#check d
#html d.toHtml

structure Prim where
  {T : Type}
  [inst : PrimInterface T]
  [strg : ToString T]
  val : T

def Prim.draw (p : Prim) (fr : ProofWidgets.Svg.Frame) : ProofWidgets.Svg.Element fr :=
  p.inst.draw p.val fr

instance : ToString Prim where
  toString p := @ToString.toString p.T p.strg p.val

#eval Prim.mk Circle.o
def prim {α : Type} [PrimInterface α] [ToString α] (a : α) : Prim := Prim.mk a
#eval prim Line.o

def fooo : Array Prim := #[⟨Circle.o⟩, ⟨Line.o⟩]

-- def foo : List Prim := [prim Circle.o, prim Line.o]
def foo : Array Prim := #[prim Circle.o, prim Line.o]

open ProofWidgets Svg in
private def svg : Svg frame :=
  { elements := Array.map (λx => Prim.draw x frame) foo}

open ProofWidgets Svg in
def drawsvg (a : Array Prim) (fr : Frame := frame) : ProofWidgets.Html :=
  let svg : ProofWidgets.Svg fr := { elements := Array.map (λx => Prim.draw x fr) a}
  svg.toHtml

#html svg.toHtml
#check svg.toHtml

-- #check Array.map prim #[prim Circle.o, prim Line.o]

-- def Prim.comp {α β : Type} [PrimInterface α] [PrimInterface β] [ToString α] [ToString β] (p1 : α) (p2 : β) : Array Prim :=
--   #[prim p1, prim p2]

class HPlus (α : Type u) (β : Type v) where
  hPlus : α → β → Array Prim

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

infixr:80 " ⊕ " => HPlus.hPlus

-- #ev(us Circle⊕ .o Circl : Array Prim)e.o
#eval Circle.o ⊕ Circle.o

#eval Circle.o ⊕ Circle.o ⊕ Line.o
#html drawsvg (Circle.o ⊕ (Circle.mk 0.5 ⊞[0,1]) ⊕ Line.o)

open ProofWidgets Svg in
private def frame2 : Frame where
  xmin   := -5
  ymin   := -5
  xSize  := 10
  width  := 400
  height := 400
#html drawsvg (Circle.o ⊕ (Circle.mk 0.5 ⊞[1,1]) ⊕ Line.o) frame2
def eyes := (Circle.mk 0.3 ⊞[-0.8,1]) ⊕ (Circle.mk 0.3 ⊞[0.8,1])
#html drawsvg (Circle.mk 2.0 ⊞[0,0] ⊕ eyes ⊕ Line.mk (⊞[-1,-0.5], ⊞[1,-0.5])) frame2

#eval (#[] : Array Prim)

end Primitives
