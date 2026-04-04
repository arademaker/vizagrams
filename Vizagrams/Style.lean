/-
Copyright (c) 2025 Henrique Borges. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Davi Barreira, Henrique Borges
-/
import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay
/-!
# Styling Primitives for Vizagrams

This module defines the data structures used for styling geometric objects.
It includes:
- `StyleSize`: A unit of length, which can be in pixels or absolute coordinates.
- `Style`: A container for optional styling attributes like stroke and fill color/width.
- An `Append` instance for `Style`, allowing composition with the `++` operator.
-/

open ProofWidgets.Svg

namespace Sty

/-- Represents a size unit, either in absolute coordinates or screen pixels. -/
inductive StyleSize where
  /-- Size in screen pixels. -/
  | px (size : Nat)
  /-- Size in abstract coordinate units defined by a `Frame`. -/
  | abs (size : Float)
deriving Repr

/-- A `Repr` instance for `ProofWidgets.Svg.Color` to enable automatic deriving. -/
instance : Repr Color where
  reprPrec c _ := s!"Color.mk {repr c.r} {repr c.g} {repr c.b}"

/--
Represents the styling attributes of a geometric object.
Each attribute is an `Option`, where `none` signifies that the attribute is unset
and should be inherited from a parent style.
-/
structure Style where
  stroke_color := (none : Option Color)
  stroke_width := (none : Option StyleSize)
  fill_color   := (none : Option Color)
deriving Repr, Inhabited

/--
Combines two styles. Attributes from the right-hand style (`s2`) override those
from the left-hand style (`s1`). This allows for chaining `style1 ++ style2 ++ ...`.
-/
instance : Append Style where
  append s1 s2 := {
    stroke_color := s2.stroke_color.orElse fun () => s1.stroke_color
    stroke_width := s2.stroke_width.orElse fun () => s1.stroke_width
    fill_color := s2.fill_color.orElse fun () => s1.fill_color
  }

/-- Converts a `StyleSize` to a `ProofWidgets.Svg.Size` within a given frame. -/
def to_svg_size (s : StyleSize) (fr : Frame) : Size fr :=
  match s with
  | .px x  => .px x
  | .abs x => .abs x

/-- Lifts the `to_svg_size` conversion to operate on `Option StyleSize`. -/
def style_to_svg_size (s : Option StyleSize) (fr : Frame) : Option (Size fr) :=
  s.map (to_svg_size · fr)

end Sty
