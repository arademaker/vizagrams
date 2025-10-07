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
# Chess Game Module

This module combines board and pieces to create a complete chess game.
Everything is self-contained in this file.
-/

-- ========== BOARD DEFINITIONS ==========

def squareSize : Float := 1.0
def lightSquareColor : Color := Color.mk 0.93 0.87 0.8
def darkSquareColor : Color := Color.mk 0.72 0.53 0.39
def labelColor : Color := Color.mk 0.2 0.2 0.2

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

def makeRow (row : Nat) : 𝕋 Mark :=
  let squares := List.range 8 |> List.map (makeSquare row)
  match squares with
  | [] => (NewCircle 0 ![0,0] : 𝕋 Mark)
  | head :: tail => tail.foldl (· + ·) head

def board8x8 : 𝕋 Mark :=
  let rows := List.range 8 |> List.map makeRow
  match rows with
  | [] => (NewCircle 0 ![0,0] : 𝕋 Mark)
  | head :: tail => tail.foldl (· + ·) head

def columnLabels : 𝕋 Mark :=
  let labels := ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
  let yPos : Float := -0.5
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

def rowLabels : 𝕋 Mark :=
  let labels := [1, 2, 3, 4, 5, 6, 7, 8]
  let xPos : Float := -0.5
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

def boardBorder : 𝕋 Mark :=
  let borderSize := 8.0 * squareSize
  let borderPoly := {geom := .polyline #[![0,0], ![0,borderSize], ![borderSize,borderSize], ![borderSize,0], ![0,0]],
                     style := {strokeColor := Color.mk 0.1 0.1 0.1, strokeWidth := StyleSize.px 3}}
  (borderPoly : Prim)

def chessBoard : 𝕋 Mark :=
  board8x8 + boardBorder + columnLabels + rowLabels

-- ========== PIECE DEFINITIONS ==========

inductive PieceColor where
  | White : PieceColor
  | Black : PieceColor
deriving Repr, BEq

inductive PieceType where
  | Pawn : PieceType
  | Rook : PieceType
  | Knight : PieceType
  | Bishop : PieceType
  | Queen : PieceType
  | King : PieceType
deriving Repr, BEq

structure Piece where
  pieceType : PieceType
  color : PieceColor
deriving Repr, BEq

def whitePieceColor : Color := Color.mk 0.95 0.95 0.95
def blackPieceColor : Color := Color.mk 0.15 0.15 0.15
def pieceOutlineColor : Color := Color.mk 0 0 0

def getPieceColor (c : PieceColor) : Color :=
  match c with
  | .White => whitePieceColor
  | .Black => blackPieceColor

def pieceScale : Float := 0.5

def pawnShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  let base := NewPolygon #[![-0.25, -0.4], ![0.25, -0.4], ![0.2, -0.2], ![-0.2, -0.2]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let body := (translate ![0, 0.1] : ℍ) * (NewCircle 0.22 ![0, 0]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 } : 𝕋 Mark)
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + body)

def rookShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  let base := NewPolygon #[![-0.3, -0.5], ![0.3, -0.5], ![0.28, -0.3], ![-0.28, -0.3]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let body := NewPolygon #[![-0.28, -0.3], ![0.28, -0.3], ![0.32, 0.3], ![-0.32, 0.3]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let cren1 := NewPolygon #[![-0.32, 0.3], ![-0.15, 0.3], ![-0.15, 0.5], ![-0.32, 0.5]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let cren2 := NewPolygon #[![-0.08, 0.3], ![0.08, 0.3], ![0.08, 0.5], ![-0.08, 0.5]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let cren3 := NewPolygon #[![0.15, 0.3], ![0.32, 0.3], ![0.32, 0.5], ![0.15, 0.5]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (body : 𝕋 Mark) + (cren1 : 𝕋 Mark) + (cren2 : 𝕋 Mark) + (cren3 : 𝕋 Mark))

def knightShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  let base := NewPolygon #[![-0.25, -0.5], ![0.25, -0.5], ![0.2, -0.3], ![-0.2, -0.3]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let head := NewPolygon #[![-0.2, -0.3], ![-0.1, 0.2], ![0, 0.5], ![0.25, 0.4], ![0.3, 0.1], ![0.2, -0.1], ![0.1, -0.3]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let eye := (translate ![0.15, 0.2] : ℍ) * (NewCircle 0.05 ![0, 0]
    { fillColor := pieceOutlineColor, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 1 } : 𝕋 Mark)
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (head : 𝕋 Mark) + eye)

def bishopShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  let base := NewPolygon #[![-0.25, -0.5], ![0.25, -0.5], ![0.2, -0.3], ![-0.2, -0.3]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let body := NewPolygon #[![-0.2, -0.3], ![0.2, -0.3], ![0.15, 0.2], ![-0.15, 0.2]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let head := NewPolygon #[![-0.15, 0.2], ![0.15, 0.2], ![0, 0.55]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let ball := (translate ![0, 0.65] : ℍ) * (NewCircle 0.08 ![0, 0]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 } : 𝕋 Mark)
  let slit := {geom := .polyline #[![-0.08, 0.05], ![0.08, 0.15]],
               style := {strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2}}
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (body : 𝕋 Mark) + (head : 𝕋 Mark) + ball + ((slit : Prim) : 𝕋 Mark))

def queenShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  let base := NewPolygon #[![-0.3, -0.5], ![0.3, -0.5], ![0.25, -0.3], ![-0.25, -0.3]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let body := NewPolygon #[![-0.25, -0.3], ![0.25, -0.3], ![0.3, 0.1], ![-0.3, 0.1]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let crown := NewPolygon #[![-0.3, 0.1], ![-0.25, 0.4], ![-0.15, 0.2], ![0, 0.5], ![0.15, 0.2], ![0.25, 0.4], ![0.3, 0.1]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let ball1 := (translate ![-0.25, 0.45] : ℍ) * (NewCircle 0.06 ![0, 0]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 } : 𝕋 Mark)
  let ball2 := (translate ![0, 0.55] : ℍ) * (NewCircle 0.06 ![0, 0]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 } : 𝕋 Mark)
  let ball3 := (translate ![0.25, 0.45] : ℍ) * (NewCircle 0.06 ![0, 0]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 } : 𝕋 Mark)
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (body : 𝕋 Mark) + (crown : 𝕋 Mark) + ball1 + ball2 + ball3)

def kingShape (c : PieceColor) : 𝕋 Mark :=
  let color := getPieceColor c
  let base := NewPolygon #[![-0.3, -0.5], ![0.3, -0.5], ![0.25, -0.3], ![-0.25, -0.3]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let body := NewPolygon #[![-0.25, -0.3], ![0.25, -0.3], ![0.3, 0.15], ![-0.3, 0.15]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let crownBase := NewPolygon #[![-0.3, 0.15], ![0.3, 0.15], ![0.25, 0.35], ![-0.25, 0.35]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let crossV := NewPolygon #[![-0.05, 0.35], ![0.05, 0.35], ![0.05, 0.65], ![-0.05, 0.65]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  let crossH := NewPolygon #[![-0.15, 0.45], ![0.15, 0.45], ![0.15, 0.55], ![-0.15, 0.55]]
    { fillColor := color, strokeColor := pieceOutlineColor, strokeWidth := StyleSize.px 2 }
  (scale pieceScale : ℍ) * ((base : 𝕋 Mark) + (body : 𝕋 Mark) + (crownBase : 𝕋 Mark) + (crossV : 𝕋 Mark) + (crossH : 𝕋 Mark))

def renderPiece (piece : Piece) : 𝕋 Mark :=
  match piece.pieceType with
  | .Pawn => pawnShape piece.color
  | .Rook => rookShape piece.color
  | .Knight => knightShape piece.color
  | .Bishop => bishopShape piece.color
  | .Queen => queenShape piece.color
  | .King => kingShape piece.color

def placePieceAt (piece : Piece) (row col : Nat) : 𝕋 Mark :=
  let x := col.toFloat * squareSize + squareSize / 2
  let y := row.toFloat * squareSize + squareSize / 2
  (translate ![x, y] : ℍ) * renderPiece piece

-- ========== GAME STATE ==========

structure Square where
  row : Nat
  col : Nat
deriving Repr, BEq

def mkSquare (row col : Nat) : Square := { row := row, col := col }

def Position := Square → Option Piece

def emptyPosition : Position := fun _ => none

def setAt (pos : Position) (s : Square) (p : Piece) : Position :=
  fun sq => if sq == s then some p else pos sq

def initialPosition : Position :=
  let empty := emptyPosition
  -- White pieces (row 0)
  let p1 := setAt empty (mkSquare 0 0) { pieceType := .Rook, color := .White }
  let p2 := setAt p1 (mkSquare 0 1) { pieceType := .Knight, color := .White }
  let p3 := setAt p2 (mkSquare 0 2) { pieceType := .Bishop, color := .White }
  let p4 := setAt p3 (mkSquare 0 3) { pieceType := .Queen, color := .White }
  let p5 := setAt p4 (mkSquare 0 4) { pieceType := .King, color := .White }
  let p6 := setAt p5 (mkSquare 0 5) { pieceType := .Bishop, color := .White }
  let p7 := setAt p6 (mkSquare 0 6) { pieceType := .Knight, color := .White }
  let p8 := setAt p7 (mkSquare 0 7) { pieceType := .Rook, color := .White }
  -- White pawns (row 1)
  let p9 := setAt p8 (mkSquare 1 0) { pieceType := .Pawn, color := .White }
  let p10 := setAt p9 (mkSquare 1 1) { pieceType := .Pawn, color := .White }
  let p11 := setAt p10 (mkSquare 1 2) { pieceType := .Pawn, color := .White }
  let p12 := setAt p11 (mkSquare 1 3) { pieceType := .Pawn, color := .White }
  let p13 := setAt p12 (mkSquare 1 4) { pieceType := .Pawn, color := .White }
  let p14 := setAt p13 (mkSquare 1 5) { pieceType := .Pawn, color := .White }
  let p15 := setAt p14 (mkSquare 1 6) { pieceType := .Pawn, color := .White }
  let p16 := setAt p15 (mkSquare 1 7) { pieceType := .Pawn, color := .White }
  -- Black pawns (row 6)
  let p17 := setAt p16 (mkSquare 6 0) { pieceType := .Pawn, color := .Black }
  let p18 := setAt p17 (mkSquare 6 1) { pieceType := .Pawn, color := .Black }
  let p19 := setAt p18 (mkSquare 6 2) { pieceType := .Pawn, color := .Black }
  let p20 := setAt p19 (mkSquare 6 3) { pieceType := .Pawn, color := .Black }
  let p21 := setAt p20 (mkSquare 6 4) { pieceType := .Pawn, color := .Black }
  let p22 := setAt p21 (mkSquare 6 5) { pieceType := .Pawn, color := .Black }
  let p23 := setAt p22 (mkSquare 6 6) { pieceType := .Pawn, color := .Black }
  let p24 := setAt p23 (mkSquare 6 7) { pieceType := .Pawn, color := .Black }
  -- Black pieces (row 7)
  let p25 := setAt p24 (mkSquare 7 0) { pieceType := .Rook, color := .Black }
  let p26 := setAt p25 (mkSquare 7 1) { pieceType := .Knight, color := .Black }
  let p27 := setAt p26 (mkSquare 7 2) { pieceType := .Bishop, color := .Black }
  let p28 := setAt p27 (mkSquare 7 3) { pieceType := .Queen, color := .Black }
  let p29 := setAt p28 (mkSquare 7 4) { pieceType := .King, color := .Black }
  let p30 := setAt p29 (mkSquare 7 5) { pieceType := .Bishop, color := .Black }
  let p31 := setAt p30 (mkSquare 7 6) { pieceType := .Knight, color := .Black }
  let p32 := setAt p31 (mkSquare 7 7) { pieceType := .Rook, color := .Black }
  p32

def renderPosition (pos : Position) : 𝕋 Mark :=
  let allSquares := (List.range 8).flatMap fun row =>
    (List.range 8).map fun col => mkSquare row col
  let pieces := allSquares.filterMap fun square =>
    match pos square with
    | none => none
    | some piece => some (placePieceAt piece square.row square.col)
  match pieces with
  | [] => (NewCircle 0 ![0, 0] : 𝕋 Mark)
  | head :: tail => tail.foldl (· + ·) head

def renderGame (pos : Position) : 𝕋 Mark :=
  chessBoard + renderPosition pos

def initialGame : 𝕋 Mark :=
  renderGame initialPosition

-- Display the initial game
#html draw₁ initialGame

-- Example: Only kings
def onlyKings : Position :=
  let empty := emptyPosition
  let p1 := setAt empty (mkSquare 0 4) { pieceType := .King, color := .White }
  let p2 := setAt p1 (mkSquare 7 4) { pieceType := .King, color := .Black }
  p2

#html draw₁ (renderGame onlyKings)

-- Example: Famous position - Scholar's Mate
def scholarsMate : Position :=
  let empty := emptyPosition
  -- White pieces
  let p1 := setAt empty (mkSquare 0 4) { pieceType := .King, color := .White }
  let p2 := setAt p1 (mkSquare 0 5) { pieceType := .Bishop, color := .White }
  let p3 := setAt p2 (mkSquare 3 7) { pieceType := .Queen, color := .White }
  -- Black pieces
  let p4 := setAt p3 (mkSquare 7 4) { pieceType := .King, color := .Black }
  let p5 := setAt p4 (mkSquare 6 4) { pieceType := .Pawn, color := .Black }
  let p6 := setAt p5 (mkSquare 6 5) { pieceType := .Pawn, color := .Black }
  p6

#html draw₁ (renderGame scholarsMate)
