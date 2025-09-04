import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay
/-!
# Styling Primitives for Vizagrams

This module defines the data structures used for styling geometric objects.
It includes:
- `StyleSize`: A unit of length, which can be in pixels or absolute coordinates.
- `Style`: A container for optional styling attributes like stroke and fill color/width.
- `Style.comp`: A composition function to override styles.
-/

open ProofWidgets.Svg

namespace Sty

/-- Represents a size unit, either in absolute coordinates or screen pixels. -/
inductive StyleSize where
  | px   (size : Nat)
  | abs  (size : Float)
deriving Repr

/-- Represents the styling attributes of a geometric object.
`none` means the attribute is unset and should be inherited. -/
structure Style where
  strokeColor := (none : Option Color)
  strokeWidth := (none : Option StyleSize)
  fillColor   := (none : Option Color)

/-- A custom `Repr` instance for pretty-printing the `Style` structure. -/
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


/-- The default empty style. It has no opinions on any attribute. -/
@[match_pattern] def Style.default : Style := {}

/--
A helper function for `Option` types. It returns the second option `o2` if it is `some`,
otherwise it falls back to the first option `o1`.
-/
def rightOption {α : Type} (o1 : Option α) (o2 : Option α) : Option α :=
  match o2 with
  | none => o1
  | some a => some a

/--
Composes two styles, `s1` and `s2`.
Attributes defined in `s2` (i.e., not `none`) will override the corresponding
attributes in `s1`. This allows for a cascading style system.
-/
def Style.comp (s1 : Style) (s2 : Style) : Style :=
  let strokeColor := rightOption s1.strokeColor s2.strokeColor
  let strokeWidth := rightOption s1.strokeWidth s2.strokeWidth
  let fillColor := rightOption s1.fillColor s2.fillColor
  {strokeColor := strokeColor, strokeWidth := strokeWidth, fillColor := fillColor}

/-- Converts a `StyleSize` to a `ProofWidgets.Svg.Size` within a given frame. -/
def styToSize (s : StyleSize) (fr : Frame) : Size fr :=
  match s with
  | StyleSize.px x => Size.px x
  | StyleSize.abs x => Size.abs x

/-- Lifts the `styToSize` function to operate on `Option StyleSize`. -/
def styleToSize (s : Option StyleSize) (fr : Frame) : Option (Size fr) :=
  match s with
  | none => none
  | some a => match a with
    | StyleSize.px x => some (Size.px x)
    | StyleSize.abs x => some (Size.abs x)

end Sty
