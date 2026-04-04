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
# Chess Pieces Module

This module defines chess pieces with:
- Piece types (Pawn, Rook, Knight, Bishop, Queen, King)
- Piece colors (White, Black)
- Geometric representations for each piece
- Functions to position pieces on the board
-/

-- Piece color
inductive PieceColor where
  | White : PieceColor
  | Black : PieceColor
deriving Repr, BEq

-- Piece type
inductive PieceType where
  | Pawn : PieceType
  | Rook : PieceType
  | Knight : PieceType
  | Bishop : PieceType
  | Queen : PieceType
  | King : PieceType
deriving Repr, BEq

-- Complete piece (type + color)
structure Piece where
  pieceType : PieceType
  color : PieceColor
deriving Repr, BEq

-- Colors for pieces
def whitePieceColor : Color := Color.mk 0.95 0.95 0.95  -- Almost white
def blackPieceColor : Color := Color.mk 0.15 0.15 0.15  -- Almost black
def pieceOutlineColor : Color := Color.mk 0 0 0         -- Black outline

-- Get color for a piece
def getPieceColor (c : PieceColor) : Color :=
  match c with
  | .White => whitePieceColor
  | .Black => blackPieceColor

-- Size reference (pieces fit in a square of size 1)
def pieceScale : Float := 0.35  -- Pieces are scaled to fit nicely

/-!
## Geometric Representations

Improved designs for each piece type:
- Pawn: Circle on small base
- Rook: Castle tower with crenellations
- Knight: Horse head profile
- Bishop: Pointed hat with cross
- Queen: Crown with multiple points
- King: Cross on crown
-/

-- Pawn: Circle on trapezoid base
def pawnShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  -- Base (trapezoid)
  let base := new_polygon #[![-0.25, -0.4], ![0.25, -0.4], ![0.2, -0.2], ![-0.2, -0.2]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Body (circle)
  let body := (translate ![0, 0.1] : ℍ) * (new_circle 0.22 ![0, 0]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 } : 𝕋 Mark)
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + body)

-- Rook: Castle tower
def rookShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  -- Base
  let base := new_polygon #[![-0.3, -0.5], ![0.3, -0.5], ![0.28, -0.3], ![-0.28, -0.3]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Body
  let body := new_polygon #[![-0.28, -0.3], ![0.28, -0.3], ![0.32, 0.3], ![-0.32, 0.3]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Top crenellations (3 rectangles)
  let cren1 := new_polygon #[![-0.32, 0.3], ![-0.15, 0.3], ![-0.15, 0.5], ![-0.32, 0.5]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  let cren2 := new_polygon #[![-0.08, 0.3], ![0.08, 0.3], ![0.08, 0.5], ![-0.08, 0.5]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  let cren3 := new_polygon #[![0.15, 0.3], ![0.32, 0.3], ![0.32, 0.5], ![0.15, 0.5]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (body : 𝕋 Mark) + (cren1 : 𝕋 Mark) + (cren2 : 𝕋 Mark) + (cren3 : 𝕋 Mark))

-- Knight: Horse head (simplified profile)
def knightShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  -- Base
  let base := new_polygon #[![-0.25, -0.5], ![0.25, -0.5], ![0.2, -0.3], ![-0.2, -0.3]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Neck and head profile
  let head := new_polygon #[![-0.2, -0.3], ![-0.1, 0.2], ![0, 0.5], ![0.25, 0.4], ![0.3, 0.1], ![0.2, -0.1], ![0.1, -0.3]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Eye (small circle)
  let eye := (translate ![0.15, 0.2] : ℍ) * (new_circle 0.05 ![0, 0]
    { fill_color := pieceOutlineColor
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 1 } : 𝕋 Mark)
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (head : 𝕋 Mark) + eye)

-- Bishop: Pointed mitre with slit
def bishopShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  -- Base
  let base := new_polygon #[![-0.25, -0.5], ![0.25, -0.5], ![0.2, -0.3], ![-0.2, -0.3]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Body (tapered)
  let body := new_polygon #[![-0.2, -0.3], ![0.2, -0.3], ![0.15, 0.2], ![-0.15, 0.2]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Head (pointed top)
  let head := new_polygon #[![-0.15, 0.2], ![0.15, 0.2], ![0, 0.55]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Small ball on top
  let ball := (translate ![0, 0.65] : ℍ) * (new_circle 0.08 ![0, 0]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 } : 𝕋 Mark)
  -- Slit in middle (diagonal line decoration)
  let slit := {geom := .polyline #[![-0.08, 0.05], ![0.08, 0.15]],
               style := {stroke_color := pieceOutlineColor, stroke_width := StyleSize.px 2}}
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (body : 𝕋 Mark) + (head : 𝕋 Mark) + (ball : 𝕋 Mark) + ((slit : Prim) : 𝕋 Mark))

-- Queen: Crown with 5 points
def queenShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  -- Base
  let base := new_polygon #[![-0.3, -0.5], ![0.3, -0.5], ![0.25, -0.3], ![-0.25, -0.3]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Body
  let body := new_polygon #[![-0.25, -0.3], ![0.25, -0.3], ![0.3, 0.1], ![-0.3, 0.1]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Crown (5 pointed top)
  let crown := new_polygon #[![-0.3, 0.1], ![-0.25, 0.4], ![-0.15, 0.2], ![0, 0.5], ![0.15, 0.2], ![0.25, 0.4], ![0.3, 0.1]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Small balls on crown points
  let ball1 := (translate ![-0.25, 0.45] : ℍ) * (new_circle 0.06 ![0, 0]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 } : 𝕋 Mark)
  let ball2 := (translate ![0, 0.55] : ℍ) * (new_circle 0.06 ![0, 0]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 } : 𝕋 Mark)
  let ball3 := (translate ![0.25, 0.45] : ℍ) * (new_circle 0.06 ![0, 0]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 } : 𝕋 Mark)
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (body : 𝕋 Mark) + (crown : 𝕋 Mark) + ball1 + ball2 + ball3)

-- King: Cross on crown
def kingShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  -- Base
  let base := new_polygon #[![-0.3, -0.5], ![0.3, -0.5], ![0.25, -0.3], ![-0.25, -0.3]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Body
  let body := new_polygon #[![-0.25, -0.3], ![0.25, -0.3], ![0.3, 0.15], ![-0.3, 0.15]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Crown base
  let crownBase := new_polygon #[![-0.3, 0.15], ![0.3, 0.15], ![0.25, 0.35], ![-0.25, 0.35]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Cross vertical
  let crossV := new_polygon #[![-0.05, 0.35], ![0.05, 0.35], ![0.05, 0.65], ![-0.05, 0.65]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  -- Cross horizontal
  let crossH := new_polygon #[![-0.15, 0.45], ![0.15, 0.45], ![0.15, 0.55], ![-0.15, 0.55]]
    { fill_color := color
    , stroke_color := pieceOutlineColor
    , stroke_width := StyleSize.px 2 }
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (body : 𝕋 Mark) + (crownBase : 𝕋 Mark) + (crossV : 𝕋 Mark) + (crossH : 𝕋 Mark))

-- Main function to render any piece
def renderPiece (piece : Piece) : 𝕋 Mark :=
  match piece.pieceType with
  | .Pawn => pawnShape piece.color
  | .Rook => rookShape piece.color
  | .Knight => knightShape piece.color
  | .Bishop => bishopShape piece.color
  | .Queen => queenShape piece.color
  | .King => kingShape piece.color

-- Helper function to place piece at board position (a1 = 0,0)
-- Position is in board coordinates (0-7, 0-7)
def placePieceAt (piece : Piece) (row col : Nat) (squareSize : Float := 1.0) : 𝕋 Mark :=
  let x := col.toFloat * squareSize + squareSize / 2  -- Center of square
  let y := row.toFloat * squareSize + squareSize / 2
  (translate ![x, y] : ℍ) * renderPiece piece

-- Test: Create some pieces
def whitePawn : Piece := { pieceType := .Pawn, color := .White }
def blackPawn : Piece := { pieceType := .Pawn, color := .Black }
def whiteRook : Piece := { pieceType := .Rook, color := .White }
def blackRook : Piece := { pieceType := .Rook, color := .Black }
def whiteKnight : Piece := { pieceType := .Knight, color := .White }
def blackKnight : Piece := { pieceType := .Knight, color := .Black }
def whiteBishop : Piece := { pieceType := .Bishop, color := .White }
def blackBishop : Piece := { pieceType := .Bishop, color := .Black }
def whiteQueen : Piece := { pieceType := .Queen, color := .White }
def blackQueen : Piece := { pieceType := .Queen, color := .Black }
def whiteKing : Piece := { pieceType := .King, color := .White }
def blackKing : Piece := { pieceType := .King, color := .Black }

-- Display all white pieces in a row
def whitePieces : 𝕋 Mark :=
  renderPiece whitePawn →[0.3]
  renderPiece whiteRook →[0.3]
  renderPiece whiteKnight →[0.3]
  renderPiece whiteBishop →[0.3]
  renderPiece whiteQueen →[0.3]
  renderPiece whiteKing

#html draw₁ whitePieces

-- Display all black pieces in a row
def blackPieces : 𝕋 Mark :=
  renderPiece blackPawn →[0.3]
  renderPiece blackRook →[0.3]
  renderPiece blackKnight →[0.3]
  renderPiece blackBishop →[0.3]
  renderPiece blackQueen →[0.3]
  renderPiece blackKing

#html draw₁ blackPieces

-- Display all pieces together (white on top, black on bottom)
def allPieces : 𝕋 Mark :=
  whitePieces ↑[0.5] blackPieces

#html draw₁ allPieces
