import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay
-- import Std.Data.HashMap

open ProofWidgets.Svg

namespace Sty
inductive StyleSize where
  | px   (size : Nat)
  | abs  (size : Float)
deriving Repr

structure Style where
  strokeColor := (none : Option Color)
  strokeWidth := (none : Option StyleSize)
  fillColor   := (none : Option Color)

instance : Repr Style where
    reprPrec s _ :=
      let strokeColorStr := match s.strokeColor with
        | none => "none"
        | some c => repr (c.r, c.g, c.b)
      let strokeWidthStr := match s.strokeWidth with
        | none => "none"
        | some w => repr w
      let fillColorStr := match s.fillColor with
        | none => "none"
        | some c => repr (c.r, c.g, c.b)
      "Style.mk { strokeColor := " ++ strokeColorStr ++ ", strokeWidth := " ++ strokeWidthStr ++ ", fillColor := " ++ fillColorStr ++ " }"

def rightOption {α : Type} (o1 : Option α) (o2 : Option α) : Option α :=
  match o2 with
  | none => o1
  | some a => some a

def Style.comp (s1 : Style) (s2 : Style) : Style :=
  let strokeColor := rightOption s1.strokeColor s2.strokeColor
  let strokeWidth := rightOption s1.strokeWidth s2.strokeWidth
  let fillColor := rightOption s1.fillColor s2.fillColor
  {strokeColor := strokeColor, strokeWidth := strokeWidth, fillColor := fillColor}

def styToSize (s : StyleSize) (fr : Frame) : Size fr :=
  match s with
  | StyleSize.px x => Size.px x
  | StyleSize.abs x => Size.abs x

private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400

private def x : StyleSize := .px 10
#eval styToSize x frame
private def y : Size frame := Size.px 10
private def exampleSizeAbs : Size frame := Size.abs 5.5

private def z : Style := {fillColor := Color.mk 0.0 0.0 0.0, strokeColor := Color.mk 1 1 1}
private def w : Style := {fillColor := Color.mk 1.0 2.0 3.0}
#eval  w

def styleToSize (s : Option StyleSize) (fr : Frame) : Option (Size fr) :=
  match s with
  | none => none
  | some a => match a with
    | StyleSize.px x => some (Size.px x)
    | StyleSize.abs x => some (Size.abs x)

#eval Style.comp z w
#eval Style.comp w z

end Sty
