import Vizagrams.FreeMonad

namespace Envelope
open GeometricPrimitive
open GraphicalPrimitive
open GraphicalMark
open FreeMonad
-- Auxiliar: calcula envelope de um array de pontos,
def envelopePts (pts : Array (Vec2)) (v : Vec2) : Float :=
  if h : pts.size > 0 then
    -- índice 0 está garantido existir
    let first := pts[0]!
    let init  := dotProd v first
    pts.foldl (fun acc p => max acc (dotProd v p )) init
  else
    0.0

def sampleQBezier (p0 c p1 : Vec2) (n : Nat) : Array Vec2 :=
  List.range n |>.map (fun i =>
    let t := Float.ofNat i / Float.ofNat (n - 1)
    let oneMinusT := 1 - t
    ScalarMul (oneMinusT ^ 2) p0 +
    ScalarMul (2 * oneMinusT * t) c +
    ScalarMul (t ^ 2) p1
  ) |>.toArray

def sampleCBezier (p0 c1 c2 p1 : Vec2) (n : Nat) : Array Vec2 :=
  List.range n |>.map (fun i =>
    let t := Float.ofNat i / Float.ofNat (n - 1)
    let oneMinusT := 1 - t
    ScalarMul (oneMinusT ^ 3) p0 +
    ScalarMul (3 * oneMinusT ^ 2 * t) c1 +
    ScalarMul (3 * oneMinusT * t ^ 2) c2 +
    ScalarMul (t ^ 3) p1
  ) |>.toArray

-- Envelope geral de qualquer `Geom` na direção `v`.
def envelope (g : Geom) (v' : Vec2) : Float :=
  let v := normalize v'
  match g with
  | .circle r c =>
      dotProd v (c + r • v)
  | .line p q =>
      max (dotProd v p) (dotProd v q)
  | .polyline pts =>
      pts.foldl (fun acc p => max acc (dotProd v p)) (dotProd v pts[0]!)
  | .polygon pts =>
      pts.foldl (fun acc p => max acc (dotProd v p)) (dotProd v pts[0]!)
  | .ellipse rx ry c =>
      -- Project the ellipse edge in direction v: center + r_dir • v
      let rxv := v 0 * rx
      let ryv := v 1 * ry
      let edge := ![rxv, ryv]
      dotProd v (c + edge)
  | .rect corner width height =>
      let p1 := corner
      let p2 := ![corner 0 + width, corner 1]
      let p3 := ![corner 0, corner 1 + height]
      let p4 := ![corner 0 + width, corner 1 + height]
      #[p1, p2, p3, p4].foldl (fun acc p => max acc (dotProd v p)) (dotProd v p1)
  | .path _ =>
      -- No semantics defined, conservatively treat as zero envelope
      0.0
  | .text pos _ size =>
      -- Assume point at position plus size as radius
      dotProd v (pos + size • v)
  | .arc rx ry c rot init final =>
      -- Amostra alguns pontos ao longo do arco e computa o envelope máximo
      let steps := 32
      let angles := List.range steps |>.map (fun i =>
        let t := Float.ofNat i / Float.ofNat (steps : Nat)
        init + t * (final - init))
      let pts := angles.map (fun θ =>
        let p := pointOnEllipse θ rx ry
        rotateVec p rot + c)
      envelopePts pts.toArray v
  | .qbezier m q => 0

  | .cbezier bpts cpts =>
      let n := bpts.size
      if h : n ≥ 2 ∧ cpts.size == 2 * (n - 1) then
        let pts :=
          (List.range (n - 1)).flatMap (fun i =>
            let c1 := cpts[2 * i]!
            let c2 := cpts[2 * i + 1]!
            (sampleCBezier (bpts[i]!) c1 c2 (bpts[i+1]!) 20).toList)
        envelopePts pts.toArray v
      else
        0.0


/-- Retângulo envoltório (bounding box) dado pelos limites mínimo e máximo em x e y. -/
structure BoundingBox where
  lower : Vec2  -- canto inferior‐esquerdo
  upper : Vec2  -- canto superior‐direito

def boundingBox (g : Geom) : BoundingBox :=
  let ex₁ := envelope g (![1.0, 0.0])
  let ex₂ := envelope g (![-1.0, 0.0])
  let ey₁ := envelope g (![0.0, 1.0])
  let ey₂ := envelope g (![0.0, -1.0])
  { lower := ![-ex₂, -ey₂]
  , upper := ![ ex₁,  ey₁] }

def BoundingBox.union (b₁ b₂ : BoundingBox) : BoundingBox :=
  let i0 : Fin 2 := Fin.mk 0 (by decide)
  let i1 : Fin 2 := Fin.mk 1 (by decide)
  let lx := min (b₁.lower i0) (b₂.lower i0)
  let ly := min (b₁.lower i1) (b₂.lower i1)
  let ux := max (b₁.upper i0) (b₂.upper i0)
  let uy := max (b₁.upper i1) (b₂.upper i1)
  { lower := ![lx, ly], upper := ![ux, uy] }


def boundingBoxGroup (gs : Array Geom) : BoundingBox :=
  if h : gs.size > 0 then
    let firstBB := boundingBox gs[0]     -- boundingBox de um único Geom
    gs.foldl (fun acc g => acc.union (boundingBox g)) firstBB
  else
    { lower := ![0.0, 0.0], upper := ![0.0, 0.0] }

def envelopePosition (g₁ : Geom) ( v : Vec2 ) ( g₂ : Geom ) (gap : Float := 0): Geom :=
  let v₁ := normalize v
  let offset := (envelope g₁ v₁) + (envelope g₂ (-v₁)) + gap
  let position := ScalarMul offset v₁
  (translate position) * g₂

/-- Coloca `g₂` à direita de `g₁`, alinhando pela direção (1,0). -/
def hStackRight (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ![1,0] g₂

/-- Coloca `g₂` à esquerda de `g₁`, alinhando pela direção (-1,0). -/
def hStackLeft (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ![-1,0] g₂

/-- Coloca `g₂` acima de `g₁`, alinhando pela direção (0,1). -/
def vStackUp (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ![0,1] g₂

/-- Coloca `g₂` abaixo de `g₁`, alinhando pela direção (0,-1). -/
def vStackDown (g₁ g₂ : Geom) : Geom :=
  envelopePosition g₁ ![0,-1] g₂

def centerGeom (g : Geom) : Geom :=
  let dx := (envelope g (![1,0]) - envelope g (![-1,0])) / 2
  let dy := (envelope g (![0,1]) - envelope g (![0,-1])) / 2
  translate (![-dx, -dy]) * g


def boundingBoxPrim (p : Prim) : BoundingBox :=
  boundingBox p.geom

/-- União de bounding‐boxes de um array de Prim. -/
def boundingBoxPrims (ps : Array Prim) : BoundingBox :=
  if h : ps.size > 0 then
    let firstBB := boundingBoxPrim ps[0]
    ps.foldl (fun acc p => acc.union (boundingBoxPrim p)) firstBB
  else
    -- Array vazio: BB degenerado na origem
    { lower := ![0.0,0.0], upper := ![0.0,0.0] }

def centerPrim (p : Prim) : Prim :=
  { geom := centerGeom p.geom, style := p.style }

def centerPrims (ps : Array Prim) : Array Prim :=
  ps.map centerPrim

def envelopePositionPrim (p₁ : Prim) (v : Vec2) (p₂ : Prim) (gap : Float := 0): Prim :=
  let g := envelopePosition p₁.geom v p₂.geom gap
  { geom := g , style := p₂.style : Prim}

def envelopeArray (A : Array Prim) (v : Vec2) : Float :=
  A.foldl (λ acc p => max acc (envelope p.geom v)) 0

def envelopePositionPrims (A : Array Prim) (v : Vec2) (p₂ : Prim) (gap : Float := 0) : Prim :=
  let v₁ := normalize v
  let offset := (envelopeArray A v₁) + (envelope p₂.geom (-v₁)) + gap
  let position := ScalarMul offset v₁
  let g := translate position * p₂.geom
  { geom := g , style := p₂.style : Prim }

def envelopePositionPrimsArray (A : Array Prim) (v : Vec2) (B : Array Prim) (gap : Float := 0) : Array Prim :=
  let v₁ := normalize v
  let offset := (envelopeArray A v₁) + (envelopeArray B (-v₁)) + gap
  let position := ScalarMul offset v₁
  B.map (fun p =>
    { geom := translate position * p.geom
      style := p.style : Prim })

/-- Coloca `p₂` à direita do array `A`, alinhando pela direção (1,0). -/
def hStackRightPrims (A : Array Prim) (p₂ : Array Prim) (gap : Float := 0) : Array Prim :=
  A ⊕ (envelopePositionPrimsArray A ![1,0] p₂ gap)

/-- Coloca `p₂` à esquerda do array `A`, alinhando pela direção (-1,0). -/
def hStackLeftPrims (A : Array Prim) (p₂ : Array Prim) (gap : Float := 0) : Array Prim :=
  A ⊕ (envelopePositionPrimsArray A ![-1,0] p₂ gap)

/-- Coloca `p₂` acima do array `A`, alinhando pela direção (0,1). -/
def vStackUpPrims (A : Array Prim) (p₂ : Array Prim) (gap : Float := 0) : Array Prim :=
  A ⊕ (envelopePositionPrimsArray A ![0,1] p₂ gap)

/-- Coloca `p₂` abaixo do array `A`, alinhando pela direção (0,-1). -/
def vStackDownPrims (A : Array Prim) (p₂ : Array Prim) (gap : Float := 0) : Array Prim :=
  A ⊕ (envelopePositionPrimsArray A ![0,-1] p₂ gap)

infixr:70 " → " => hStackRightPrims
infixr:70 " ← " => hStackLeftPrims
infixr:70 " ↑ " => vStackUpPrims
infixr:70 " ↓ " => vStackDownPrims

-- notação “A →[g] B” para usar gap = g
notation:70 A " →[" g "] " B => hStackRightPrims A B g
notation:70 A " ←[" g "] " B => hStackLeftPrims  A B g
notation:70 A " ↑[" g "] " B => vStackUpPrims    A B g
notation:70 A " ↓[" g "] " B => vStackDownPrims  A B g

/--
  Bounding‐box de um Mark, via sua interface θ : T → Array Prim.
  Basta agregar os prims retornados por θ.
-/
def boundingBoxMark {T : Type} [inst : MarkInterface T] (m : T) : BoundingBox :=
  boundingBoxPrims (inst.θ m)

def boundingBoxOfMark (m : Mark) : BoundingBox :=
  boundingBoxPrims m.θ


def boundingBox𝕋 (t : 𝕋 Mark) : BoundingBox :=
  boundingBoxPrims (flat t)

def envelopePositionMarks (𝕄₁ : 𝕋 Mark) ( v : Vec2) (𝕄₂ : 𝕋 Mark) (gap : Float := 0): 𝕋 Mark :=
  let 𝕞₁ := flat 𝕄₁
  let 𝕞₂ := flat 𝕄₂
  let v₁ := normalize v
  let offset := (envelopeArray 𝕞₁ v₁) + (envelopeArray 𝕞₂ (-v₁)) + gap
  let position := ScalarMul offset v₁
  let h : ℍ := { s := {} , g := translate position }
  h * 𝕄₂

/-- Coloca `p₂` à direita do array `A`, alinhando pela direção (1,0). -/
def hStackRightMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ![1,0] 𝕄₂ gap)

/-- Coloca `p₂` à direita do array `A`, alinhando pela direção (1,0). -/
def hStackLeftMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ![-1,0] 𝕄₂ gap)

def vStackUpMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ![0,1] 𝕄₂ gap)

def vStackDownMarks (𝕄₁ : 𝕋 Mark) (𝕄₂ : 𝕋 Mark) (gap : Float := 0) : 𝕋 Mark :=
  𝕄₁ + (envelopePositionMarks 𝕄₁ ![0,-1] 𝕄₂ gap)

infixr:70 " → " => hStackRightMarks
infixr:70 " ← " => hStackLeftMarks
infixr:70 " ↑ " => vStackUpMarks
infixr:70 " ↓ " => vStackDownMarks

notation:70 A " →[" g "] " B => hStackRightMarks A B g
notation:70 A " ←[" g "] " B => hStackLeftMarks  A B g
notation:70 A " ↑[" g "] " B => vStackUpMarks    A B g
notation:70 A " ↓[" g "] " B => vStackDownMarks  A B g
notation:70 A " ↗[" v ", " g "]" B => A + envelopePositionMarks A v B g

end Envelope
