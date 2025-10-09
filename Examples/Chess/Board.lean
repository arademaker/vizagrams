import Vizagrams

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad
open LinearAlgebra
open Envelope
open Sty

/-!
# Chess Board Module

This module defines the chess board rendering with:
- 8x8 alternating colored squares (white and brown)
- Column labels (a-h) at the bottom
- Row labels (1-8) on the left side
-/

-- Square size in units
def squareSize : Float := 1.0

-- Board colors
def lightSquareColor : Color := Color.mk 0.93 0.87 0.8  -- Beige/light
def darkSquareColor : Color := Color.mk 0.72 0.53 0.39  -- Brown
def labelColor : Color := Color.mk 0.0 0.0 0.0          -- Dark gray

-- Create a single square at position (row, col) with appropriate color
-- row and col are 0-indexed (0-7)
def makeSquare (row col : Nat) : 𝕋 Mark :=
  let isLight := (row + col) % 2 == 0
  let color := if isLight then lightSquareColor else darkSquareColor
  let x := col.toFloat * squareSize
  let y := row.toFloat * squareSize
  let square := NewPolygon #[![0,0], ![0,squareSize], ![squareSize,squareSize], ![squareSize,0]]
    { fillColor := color
    , strokeColor := Color.mk 0.5 0.5 0.5
    , strokeWidth := StyleSize.px 1 }
  (translate ![x, y] : ℍ) * (square : 𝕋 Mark)

-- Create a row of 8 squares
def makeRow (row : Nat) : 𝕋 Mark :=
  let squares := List.range 8 |> List.map (makeSquare row)
  match squares with
  | [] => (NewCircle 0 ![0,0] : 𝕋 Mark)  -- Should never happen
  | head :: tail => tail.foldl (· + ·) head

-- Create the complete 8x8 board
def board8x8 : 𝕋 Mark :=
  let rows := List.range 8 |> List.map makeRow
  match rows with
  | [] => (NewCircle 0 ![0,0] : 𝕋 Mark)  -- Should never happen
  | head :: tail => tail.foldl (· + ·) head

-- Column labels (a-h)
def columnLabels : 𝕋 Mark :=
  let labels := ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
  let yPos : Float := -0.5  -- Position below the board
  let labelMarks := labels.zipIdx.map (fun (c, i) =>
    let xPos := i.toFloat * squareSize + squareSize / 2
    let style : ℍ := ℍ.mk
      { fillColor := labelColor }
      (translate ![xPos, yPos])
    style * (NewText c.toString ![0,0] 0.3 {fillColor := labelColor} : 𝕋 Mark)
  )
  match labelMarks with
  | [] => (NewCircle 0 ![0,0] : 𝕋 Mark)
  | head :: tail => tail.foldl (· + ·) head

-- Row labels (1-8)
def rowLabels : 𝕋 Mark :=
  let labels := [1, 2, 3, 4, 5, 6, 7, 8]
  let xPos : Float := -0.5  -- Position to the left of the board
  let labelMarks := labels.zipIdx.map (fun (num, i) =>
    let yPos := i.toFloat * squareSize + squareSize / 2
    let style : ℍ := ℍ.mk
      { fillColor := labelColor }
      (translate ![xPos, yPos])
    style * (NewText (toString num) ![0,0] 0.3 {fillColor := labelColor} : 𝕋 Mark)
  )
  match labelMarks with
  | [] => (NewCircle 0 ![0,0] : 𝕋 Mark)
  | head :: tail => tail.foldl (· + ·) head

-- Border around the board
def boardBorder : 𝕋 Mark :=
  let borderSize := 8.0 * squareSize
  NewPolygon #[![0,0], ![0,borderSize], ![borderSize,borderSize], ![borderSize,0]]
    { strokeColor := Color.mk 0.1 0.1 0.1
      strokeWidth := StyleSize.px 3 }

-- Complete board with labels and border
def chessBoard : 𝕋 Mark :=
  board8x8 + boardBorder + columnLabels + rowLabels

-- Test: Display single squares to verify colors
def testSquare1 : 𝕋 Mark := makeSquare 0 0  -- Should be light
def testSquare2 : 𝕋 Mark := makeSquare 0 1  -- Should be dark

#html draw₁ (testSquare1 + testSquare2)

-- Display the board
#html draw₁ chessBoard
