import Vizagrams
import ProofWidgets.Data.Html

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad

--   
def data₁ : List Float :=  [1.0, 2.3, 0.7, 3.1, 0.3]

def barPolygon (h : Float) (w : Float := 0.8) : Prim :=
  let pts : Array Vec2 :=
    #[![0, 0], ![w, 0], ![w, h], ![0, h]]
  NewPolygon pts {fillColor := Color.mk (3*h/2) (2*h/3) (h/5) }

def coalgbar (τ : List Float) : List ( List Float ) :=
  τ.map (fun x => [x])

def barExpr : List Float → 𝕋 Mark
  | [h] => 𝕋.pure (barPolygon h)
  | _   => 𝕋.pure (Nil.mk)

def algBar (τ : List (𝕋 Mark)) : 𝕋 Mark :=
  match τ with
  | x :: xs => List.foldl (· →[0.5] ·) x xs
  | [] => 𝕋.pure Nil.mk

def bars : GraphicExpression (List Float):= {
  expr := barExpr
  coalg := coalgbar
  alg := algBar
}

-- Função auxiliar para converter Float ∈ ℝ em canal RGB (0–255)
def floatToRgb (x : Float) : Nat :=
  let v := min (max x 0.0) 1.0 * 255.0
  v.toUInt8.toNat

def createSvgFromData (data : List Float) : String :=
  let header := "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"400\" height=\"300\">"
  let footer := "</svg>"
  let maxHeight := data.foldl max 0.0
  let indexedData := data.enum

  let bars := indexedData.foldl (fun acc (index, h) =>
    let x      := index * 60 + 30
    let height := (h / maxHeight) * 200
    let y      := 250 - height

    let rF := 3.0 * h / 2.0
    let gF := 2.0 * h / 3.0
    let bF := h / 5.0

    let r := floatToRgb rF
    let g := floatToRgb gF
    let b := floatToRgb bF

    let strokeR := ((r / 2) : Nat)
    let strokeG := ((g / 2) : Nat)
    let strokeB := ((b / 2) : Nat)

    let fillColor   := s!"rgb({r},{g},{b})"
    let strokeColor := s!"rgb({strokeR},{strokeG},{strokeB})"

    let barSvg := s!"<rect x=\"{x}\" y=\"{y}\" width=\"40\" height=\"{height}\" fill=\"{fillColor}\" stroke=\"{strokeColor}\"/>"
    -- let label  := s!"<text x=\"{x + 20}\" y=\"270\" text-anchor=\"middle\" font-size=\"12\">{h}</text>"

    acc ++ barSvg ++ "\n" --++ label ++ "\n"
  ) ""

  header ++ "\n" ++ bars ++ footer


def createRealSvg (data : List Float) (filename : String) : IO Unit := do
  let svgContent := createSvgFromData data
  IO.FS.writeFile filename svgContent
  --IO.println s!"SVG salvo em: {filename}"

-- Salva o .svg arquivo
--#eval createRealSvg data₁ "barchart.svg"

#html draw (bars.eval data₁)
